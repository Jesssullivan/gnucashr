#' Intercompany Elimination Functions
#'
#' Functions for handling intercompany transaction eliminations
#' in consolidated financial statements.
#'
#' @name intercompany
NULL

#' Apply Intercompany Eliminations
#'
#' Remove intercompany transactions from combined trial balance.
#'
#' @param tb Combined trial balance tibble with book_name column
#' @param ic_rules List of IC elimination rules
#' @return tibble with IC transactions eliminated
#' @export
apply_ic_eliminations <- function(tb, ic_rules) {
  if (length(ic_rules) == 0) {
    return(tb)
  }

  # Create elimination entries for each rule
  eliminations <- purrr::map_dfr(ic_rules, function(rule) {
    # Find matching entries on both sides
    from_entries <- tb |>
      dplyr::filter(
        book_name == rule$from_book,
        grepl(rule$from_account, account, fixed = TRUE)
      )

    to_entries <- tb |>
      dplyr::filter(
        book_name == rule$to_book,
        grepl(rule$to_account, account, fixed = TRUE)
      )

    if (nrow(from_entries) == 0 && nrow(to_entries) == 0) {
      return(tibble::tibble())
    }

    # Create elimination entries (reverse sign)
    elim <- dplyr::bind_rows(
      from_entries |>
        dplyr::mutate(
          debit = -debit,
          credit = -credit,
          balance = -balance,
          book_name = "ELIMINATION"
        ),
      to_entries |>
        dplyr::mutate(
          debit = -debit,
          credit = -credit,
          balance = -balance,
          book_name = "ELIMINATION"
        )
    )

    elim
  })

  # Add elimination entries to trial balance
  dplyr::bind_rows(tb, eliminations)
}

#' Create Standard IC Rule Set
#'
#' Generate standard intercompany elimination rules for a
#' parent company with multiple subsidiaries.
#'
#' @param parent_name Name of parent company book
#' @param subsidiary_names Character vector of subsidiary book names
#' @param parent_ar_pattern Pattern for parent's receivable accounts
#' @param sub_ap_pattern Pattern for subsidiary's payable accounts
#' @param mgmt_fee_income Pattern for management fee income
#' @param mgmt_fee_expense Pattern for management fee expense
#' @return List of IC rules
#' @export
create_standard_ic_rules <- function(
    parent_name = "inc",
    subsidiary_names = c("products", "services", "software", "operations"),
    parent_ar_pattern = "Assets:Due from %s",
    sub_ap_pattern = "Liabilities:Due to Parent",
    mgmt_fee_income = "Income:Management Fees:%s",
    mgmt_fee_expense = "Expenses:Management Fees"
) {

  rules <- list()

  for (sub in subsidiary_names) {
    sub_title <- tools::toTitleCase(sub)

    # Due from/to rules
    rules <- c(rules, list(list(
      from_book = parent_name,
      from_account = sprintf(parent_ar_pattern, paste0(sub_title, " LLC")),
      to_book = sub,
      to_account = sub_ap_pattern,
      description = paste("IC AR/AP:", parent_name, "<->", sub)
    )))

    # Management fee rules
    rules <- c(rules, list(list(
      from_book = parent_name,
      from_account = sprintf(mgmt_fee_income, sub_title),
      to_book = sub,
      to_account = mgmt_fee_expense,
      description = paste("IC Mgmt Fee:", parent_name, "<->", sub)
    )))
  }

  rules
}

#' Identify Uneliminated IC Transactions
#'
#' Find intercompany transactions that should have been eliminated
#' but weren't matched by existing rules.
#'
#' @param collection BookCollection object
#' @param ic_keywords Keywords that indicate intercompany accounts
#' @return tibble of potentially uneliminated IC transactions
#' @export
identify_uneliminated_ic <- function(
    collection,
    ic_keywords = c("due from", "due to", "intercompany", "affiliate",
                    "management fee", "license fee", "inter-co")
) {
  # Get all accounts across all books

all_accounts <- collection$all_accounts()

  # Find accounts that match IC keywords
  ic_pattern <- paste(tolower(ic_keywords), collapse = "|")
  ic_accounts <- all_accounts |>
    dplyr::filter(
      purrr::map_lgl(name, function(n) {
        grepl(ic_pattern, tolower(n))
      })
    )

  if (nrow(ic_accounts) == 0) {
    return(tibble::tibble(
      book_name = character(),
      account = character(),
      type = character(),
      potential_ic = logical(),
      suggested_counterparty = character()
    ))
  }

  # For each IC account, check if it has a matching elimination rule
  ic_accounts |>
    dplyr::mutate(
      potential_ic = TRUE,
      suggested_counterparty = dplyr::case_when(
        grepl("due from", tolower(name)) ~ "Look for 'due to' in other books",
        grepl("due to", tolower(name)) ~ "Look for 'due from' in other books",
        grepl("management fee", tolower(name)) & account_type == "INCOME" ~
          "Look for management fee expense in other books",
        grepl("management fee", tolower(name)) & account_type == "EXPENSE" ~
          "Look for management fee income in parent",
        TRUE ~ "Review for IC relationship"
      )
    ) |>
    dplyr::select(book_name, name, account_type, potential_ic, suggested_counterparty)
}

#' Calculate IC Elimination Journal Entry
#'
#' Generate the journal entry needed to eliminate a specific IC balance.
#'
#' @param collection BookCollection object
#' @param rule Single IC rule
#' @param as_of Date for balances
#' @return tibble representing the elimination journal entry
#' @export
calculate_ic_elimination_entry <- function(collection, rule, as_of = Sys.Date()) {
  as_of <- as.Date(as_of)

  from_book <- collection$get_book(rule$from_book)
  to_book <- collection$get_book(rule$to_book)

  if (is.null(from_book) || is.null(to_book)) {
    rlang::abort("One or both books in IC rule not found")
  }

  # Get the balance to eliminate
  from_tb <- trial_balance(from_book, as_of)
  from_balance <- from_tb |>
    dplyr::filter(grepl(rule$from_account, account, fixed = TRUE)) |>
    dplyr::summarize(balance = sum(balance, na.rm = TRUE)) |>
    dplyr::pull(balance)

  if (length(from_balance) == 0 || abs(from_balance) < 0.01) {
    return(tibble::tibble(
      description = "No elimination needed",
      account = character(),
      debit = numeric(),
      credit = numeric()
    ))
  }

  # Create elimination entry
  # Reverse the from side and to side
  tibble::tibble(
    description = paste("IC Elimination:", rule$description),
    account = c(
      paste0("[ELIM] ", rule$from_account),
      paste0("[ELIM] ", rule$to_account)
    ),
    debit = c(
      if (from_balance > 0) 0 else abs(from_balance),
      if (from_balance > 0) from_balance else 0
    ),
    credit = c(
      if (from_balance > 0) from_balance else 0,
      if (from_balance > 0) 0 else abs(from_balance)
    )
  )
}
