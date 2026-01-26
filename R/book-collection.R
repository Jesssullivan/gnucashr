#' BookCollection Class
#'
#' R6 class for managing multiple GnuCash books with consolidation support.
#' Enables multi-entity financial modeling with intercompany elimination.
#'
#' @export
BookCollection <- R6::R6Class("BookCollection",
  public = list(
    #' @description Create a new BookCollection
    #' @param entity_config Optional entity configuration from load_entity_config()
    initialize = function(entity_config = NULL) {
      private$books <- list()
      private$entity_config <- entity_config
      private$ic_rules <- list()
      invisible(self)
    },

    #' @description Add a book to the collection
    #' @param name Unique identifier for this book (e.g., "inc", "products")
    #' @param path Path to GnuCash file
    #' @param read_only Open in read-only mode (default TRUE)
    add_book = function(name, path, read_only = TRUE) {
      if (name %in% names(private$books)) {
        rlang::warn(paste("Book", name, "already exists; replacing."))
      }

      gc <- read_gnucash(path, read_only = read_only)
      private$books[[name]] <- gc
      invisible(self)
    },

    #' @description Remove a book from the collection
    #' @param name Book identifier to remove
    remove_book = function(name) {
      if (name %in% names(private$books)) {
        private$books[[name]]$close()
        private$books[[name]] <- NULL
      }
      invisible(self)
    },

    #' @description Get a specific book
    #' @param name Book identifier
    get_book = function(name) {
      private$books[[name]]
    },

    #' @description List all books in the collection
    list_books = function() {
      if (length(private$books) == 0) {
        return(tibble::tibble(
          name = character(),
          path = character(),
          format = character(),
          currency = character(),
          n_accounts = integer(),
          n_transactions = integer()
        ))
      }

      purrr::map_dfr(names(private$books), function(name) {
        gc <- private$books[[name]]
        meta <- gc$metadata()
        tibble::tibble(
          name = name,
          path = meta$path,
          format = meta$format,
          currency = meta$default_currency,
          n_accounts = nrow(gc$accounts(collected = TRUE)),
          n_transactions = nrow(gc$transactions(collected = TRUE))
        )
      })
    },

    #' @description Apply a function to each book (monadic each)
    #' @param fn Function to apply, receives (book, name) arguments
    #' @return List of results keyed by book name
    each = function(fn) {
      purrr::imap(private$books, fn)
    },

    #' @description Get combined accounts from all books
    #' @param with_book_name Add book_name column (default TRUE)
    all_accounts = function(with_book_name = TRUE) {
      self$each(function(book, name) {
        accounts <- book$accounts(collected = TRUE)
        if (with_book_name) {
          accounts <- accounts |>
            dplyr::mutate(book_name = name, .before = 1)
        }
        accounts
      }) |>
        dplyr::bind_rows()
    },

    #' @description Get combined transactions from all books
    #' @param with_book_name Add book_name column (default TRUE)
    all_transactions = function(with_book_name = TRUE) {
      self$each(function(book, name) {
        txns <- book$transactions(collected = TRUE)
        if (with_book_name) {
          txns <- txns |>
            dplyr::mutate(book_name = name, .before = 1)
        }
        txns
      }) |>
        dplyr::bind_rows()
    },

    #' @description Get combined splits from all books
    #' @param with_book_name Add book_name column (default TRUE)
    all_splits = function(with_book_name = TRUE) {
      self$each(function(book, name) {
        splits <- book$splits(collected = TRUE)
        if (with_book_name) {
          splits <- splits |>
            dplyr::mutate(book_name = name, .before = 1)
        }
        splits
      }) |>
        dplyr::bind_rows()
    },

    #' @description Add intercompany elimination rule
    #' @param from_book Source book name
    #' @param from_account Source account path pattern
    #' @param to_book Target book name
    #' @param to_account Target account path pattern
    #' @param description Rule description
    add_ic_rule = function(from_book, from_account, to_book, to_account, description = NULL) {
      rule <- list(
        from_book = from_book,
        from_account = from_account,
        to_book = to_book,
        to_account = to_account,
        description = description %||% paste(from_book, "->", to_book)
      )
      private$ic_rules <- c(private$ic_rules, list(rule))
      invisible(self)
    },

    #' @description Get all intercompany rules
    get_ic_rules = function() {
      if (length(private$ic_rules) == 0) {
        return(tibble::tibble(
          from_book = character(),
          from_account = character(),
          to_book = character(),
          to_account = character(),
          description = character()
        ))
      }
      dplyr::bind_rows(private$ic_rules)
    },

    #' @description Set up default IC rules for Tinyland structure
    setup_tinyland_ic_rules = function() {
      # Parent due from/to subsidiaries
      llcs <- c("products", "services", "software", "operations")

      for (llc in llcs) {
        # Due from parent to LLC
        self$add_ic_rule(
          "inc", paste0("Assets:Due from ", tools::toTitleCase(llc), " LLC"),
          llc, "Liabilities:Due to Tinyland Inc",
          paste("IC: Inc -> ", tools::toTitleCase(llc))
        )

        # Management fees
        self$add_ic_rule(
          "inc", paste0("Income:Management Fees:", tools::toTitleCase(llc)),
          llc, "Expenses:Management Fees",
          paste("IC: Mgmt fee Inc <-", tools::toTitleCase(llc))
        )
      }

      # Software licensing (Services -> Software)
      self$add_ic_rule(
        "services", "Expenses:License Fees:Software LLC",
        "software", "Income:License Fees:Services LLC",
        "IC: License Services -> Software"
      )

      # Infrastructure fees (Services -> Operations)
      self$add_ic_rule(
        "services", "Expenses:Infrastructure:Operations LLC",
        "operations", "Income:Infrastructure Services:Services LLC",
        "IC: Infra Services -> Operations"
      )

      invisible(self)
    },

    #' @description Generate consolidated trial balance
    #' @param as_of Date for balance calculation (default: today)
    #' @param eliminate_ic Apply intercompany eliminations (default TRUE)
    consolidated_trial_balance = function(as_of = Sys.Date(), eliminate_ic = TRUE) {
      as_of <- as.Date(as_of)

      # Get trial balance from each book
      tb_list <- self$each(function(book, name) {
        trial_balance(book, as_of) |>
          dplyr::mutate(book_name = name, .before = 1)
      })

      combined <- dplyr::bind_rows(tb_list)

      if (eliminate_ic && length(private$ic_rules) > 0) {
        combined <- apply_ic_eliminations(combined, private$ic_rules)
      }

      # Aggregate by account across books
      combined |>
        dplyr::group_by(account, type) |>
        dplyr::summarize(
          debit = sum(debit, na.rm = TRUE),
          credit = sum(credit, na.rm = TRUE),
          balance = sum(balance, na.rm = TRUE),
          books = paste(unique(book_name), collapse = ", "),
          .groups = "drop"
        ) |>
        dplyr::arrange(type, account)
    },

    #' @description Generate consolidated balance sheet
    #' @param as_of Date for balance calculation
    #' @param eliminate_ic Apply intercompany eliminations
    consolidated_balance_sheet = function(as_of = Sys.Date(), eliminate_ic = TRUE) {
      tb <- self$consolidated_trial_balance(as_of, eliminate_ic)

      balance_sheet(tb)
    },

    #' @description Generate consolidated income statement
    #' @param start_date Start of period
    #' @param end_date End of period
    #' @param eliminate_ic Apply intercompany eliminations
    consolidated_income_statement = function(start_date, end_date = Sys.Date(), eliminate_ic = TRUE) {
      # For income statement, we need period activity not cumulative balance
      end_date <- as.Date(end_date)
      start_date <- as.Date(start_date)

      # Get income/expense activity for the period
      activity <- self$each(function(book, name) {
        income_statement_activity(book, start_date, end_date) |>
          dplyr::mutate(book_name = name, .before = 1)
      }) |>
        dplyr::bind_rows()

      if (eliminate_ic && length(private$ic_rules) > 0) {
        activity <- apply_ic_eliminations(activity, private$ic_rules)
      }

      income_statement(activity)
    },

    #' @description Intercompany reconciliation report
    #' @param as_of Date for reconciliation
    intercompany_reconciliation = function(as_of = Sys.Date()) {
      if (length(private$ic_rules) == 0) {
        rlang::inform("No IC rules defined. Use add_ic_rule() or setup_tinyland_ic_rules().")
        return(tibble::tibble())
      }

      as_of <- as.Date(as_of)

      # For each IC rule, get balances from both sides
      purrr::map_dfr(private$ic_rules, function(rule) {
        from_book <- private$books[[rule$from_book]]
        to_book <- private$books[[rule$to_book]]

        if (is.null(from_book) || is.null(to_book)) {
          return(tibble::tibble(
            rule = rule$description,
            from_balance = NA_real_,
            to_balance = NA_real_,
            difference = NA_real_,
            status = "Missing book"
          ))
        }

        # Get balances
        from_tb <- trial_balance(from_book, as_of)
        to_tb <- trial_balance(to_book, as_of)

        from_balance <- from_tb |>
          dplyr::filter(grepl(rule$from_account, account, fixed = TRUE)) |>
          dplyr::summarize(balance = sum(balance, na.rm = TRUE)) |>
          dplyr::pull(balance)

        to_balance <- to_tb |>
          dplyr::filter(grepl(rule$to_account, account, fixed = TRUE)) |>
          dplyr::summarize(balance = sum(balance, na.rm = TRUE)) |>
          dplyr::pull(balance)

        from_balance <- if (length(from_balance) == 0) 0 else from_balance
        to_balance <- if (length(to_balance) == 0) 0 else to_balance

        tibble::tibble(
          rule = rule$description,
          from_book = rule$from_book,
          from_account = rule$from_account,
          from_balance = from_balance,
          to_book = rule$to_book,
          to_account = rule$to_account,
          to_balance = to_balance,
          # IC should net to zero (from - to = 0 OR from + to = 0 depending on normal balance)
          difference = from_balance + to_balance,
          status = if (abs(from_balance + to_balance) < 0.01) "Balanced" else "OUT OF BALANCE"
        )
      })
    },

    #' @description Close all books
    close = function() {
      for (name in names(private$books)) {
        private$books[[name]]$close()
      }
      private$books <- list()
      invisible(self)
    },

    #' @description Print method
    print = function() {
      cat("<BookCollection>\n")
      cat("  Books:", length(private$books), "\n")
      if (length(private$books) > 0) {
        cat("  Names:", paste(names(private$books), collapse = ", "), "\n")
      }
      cat("  IC Rules:", length(private$ic_rules), "\n")
      if (!is.null(private$entity_config)) {
        cat("  Entity Config: loaded\n")
      }
      invisible(self)
    }
  ),

  private = list(
    books = NULL,
    entity_config = NULL,
    ic_rules = NULL,

    # Destructor - moved to private per R6 2.4.0 requirement
    finalize = function() {
      self$close()
    }
  )
)


#' Create a Book Collection
#'
#' Convenience function to create a BookCollection instance.
#'
#' @param entity_config Optional entity configuration
#' @return A BookCollection object
#' @export
book_collection <- function(entity_config = NULL) {
  BookCollection$new(entity_config)
}
