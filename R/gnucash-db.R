#' GnuCash Database Connection
#'
#' R6 class for managing connections to GnuCash files (SQLite or XML format).
#' Provides unified interface for both file formats with lazy evaluation.
#'
#' @export
GnuCashDB <- R6::R6Class("GnuCashDB",
  public = list(
    #' @description Create a new GnuCashDB connection
    #' @param path Path to GnuCash file (.gnucash SQLite or XML)
    #' @param read_only Open in read-only mode (default TRUE)
    initialize = function(path, read_only = TRUE) {
      if (!file.exists(path)) {
        rlang::abort(paste("GnuCash file not found:", path))
      }

      private$path <- normalizePath(path)
      private$read_only <- read_only
      private$format <- detect_gnucash_format(path)

      if (private$format == "unknown") {
        rlang::abort("Unknown GnuCash file format. Expected SQLite or XML.")
      }

      if (private$format == "sqlite") {
        private$init_sqlite()
      } else {
        private$init_xml()
      }

      invisible(self)
    },

    #' @description Get accounts table (lazy for SQLite, tibble for XML)
    #' @param collected If TRUE, always return collected tibble
    accounts = function(collected = FALSE) {
      if (private$format == "sqlite" && !collected) {
        dplyr::tbl(private$con, "accounts")
      } else {
        private$cache$accounts
      }
    },

    #' @description Get transactions table
    #' @param collected If TRUE, always return collected tibble
    transactions = function(collected = FALSE) {
      if (private$format == "sqlite" && !collected) {
        dplyr::tbl(private$con, "transactions")
      } else {
        private$cache$transactions
      }
    },

    #' @description Get splits table
    #' @param collected If TRUE, always return collected tibble
    splits = function(collected = FALSE) {
      if (private$format == "sqlite" && !collected) {
        dplyr::tbl(private$con, "splits")
      } else {
        private$cache$splits
      }
    },

    #' @description Get commodities table
    #' @param collected If TRUE, always return collected tibble
    commodities = function(collected = FALSE) {
      if (private$format == "sqlite" && !collected) {
        dplyr::tbl(private$con, "commodities")
      } else {
        private$cache$commodities
      }
    },

    #' @description Get prices table
    #' @param collected If TRUE, always return collected tibble
    prices = function(collected = FALSE) {
      if (private$format == "sqlite" && !collected) {
        dplyr::tbl(private$con, "prices")
      } else {
        private$cache$prices
      }
    },

    #' @description Get a specific account by GUID or path
    #' @param identifier Account GUID or colon-separated path (e.g., "Assets:Bank:Checking")
    get_account = function(identifier) {
      # Check if it looks like a GUID (32 hex chars) or a path
      if (grepl("^[a-f0-9]{32}$", identifier, ignore.case = TRUE)) {
        result <- self$accounts(collected = TRUE) |>
          dplyr::filter(guid == identifier)
      } else {
        # Path lookup - need to build full paths first
        tree <- self$account_tree()
        result <- tree |>
          dplyr::filter(full_path == identifier | name == identifier)
      }

      if (nrow(result) == 0) {
        return(NULL)
      }
      result[1, ]
    },

    #' @description Get a specific transaction by GUID
    #' @param guid Transaction GUID
    get_transaction = function(guid) {
      result <- self$transactions(collected = TRUE) |>
        dplyr::filter(guid == !!guid)

      if (nrow(result) == 0) {
        return(NULL)
      }

      # Also get splits
      splits <- self$splits(collected = TRUE) |>
        dplyr::filter(tx_guid == !!guid)

      list(
        transaction = result[1, ],
        splits = splits
      )
    },

    #' @description Build account tree with full paths
    account_tree = function() {
      accounts <- self$accounts(collected = TRUE) |>
        dplyr::select(
          guid, name, account_type, parent_guid,
          dplyr::any_of(c("commodity_guid", "description", "hidden", "placeholder"))
        )

      # Build full paths using recursive lookup
      build_path <- function(guid, accounts, path = "") {
        row <- accounts[accounts$guid == guid, ]
        if (nrow(row) == 0 || is.na(row$parent_guid)) {
          return(path)
        }
        new_path <- if (path == "") row$name else paste(row$name, path, sep = ":")
        build_path(row$parent_guid, accounts, new_path)
      }

      accounts |>
        dplyr::rowwise() |>
        dplyr::mutate(
          full_path = build_path(guid, accounts)
        ) |>
        dplyr::ungroup() |>
        dplyr::arrange(full_path)
    },

    #' @description Get book metadata
    metadata = function() {
      list(
        path = private$path,
        format = private$format,
        book_guid = private$book_guid,
        root_account_guid = private$root_account_guid,
        default_currency = private$default_currency,
        read_only = private$read_only
      )
    },

    #' @description Check if database is connected/loaded
    is_connected = function() {
      if (private$format == "sqlite") {
        !is.null(private$con) && DBI::dbIsValid(private$con)
      } else {
        !is.null(private$cache)
      }
    },

    #' @description Refresh cached data from database
    refresh = function() {
      if (private$format == "sqlite" && !is.null(private$con) && DBI::dbIsValid(private$con)) {
        private$cache <- list(
          accounts = dplyr::collect(dplyr::tbl(private$con, "accounts")),
          transactions = dplyr::collect(dplyr::tbl(private$con, "transactions")),
          splits = dplyr::collect(dplyr::tbl(private$con, "splits")),
          commodities = dplyr::collect(dplyr::tbl(private$con, "commodities")),
          prices = if ("prices" %in% DBI::dbListTables(private$con)) {
            dplyr::collect(dplyr::tbl(private$con, "prices"))
          } else {
            tibble::tibble(
              guid = character(), commodity_guid = character(),
              currency_guid = character(), date = as.POSIXct(character()),
              source = character(), value_num = integer(), value_denom = integer()
            )
          }
        )
      }
      invisible(self)
    },

    #' @description Close the database connection
    close = function() {
      if (private$format == "sqlite" && !is.null(private$con)) {
        DBI::dbDisconnect(private$con)
        private$con <- NULL
      }
      private$cache <- NULL
      invisible(self)
    },

    #' @description Print method
    print = function() {
      cat("<GnuCashDB>\n")
      cat("  Path:", private$path, "\n")
      cat("  Format:", private$format, "\n")
      cat("  Currency:", private$default_currency, "\n")

      if (self$is_connected()) {
        n_accounts <- nrow(self$accounts(collected = TRUE))
        n_transactions <- nrow(self$transactions(collected = TRUE))
        cat("  Accounts:", n_accounts, "\n")
        cat("  Transactions:", n_transactions, "\n")
      } else {
        cat("  Status: Not connected\n")
      }

      invisible(self)
    }
  ),

  private = list(
    path = NULL,
    con = NULL,
    cache = NULL,
    format = NULL,
    book_guid = NULL,
    root_account_guid = NULL,
    default_currency = NULL,
    read_only = TRUE,

    # Destructor - moved to private per R6 2.4.0 requirement
    finalize = function() {
      self$close()
    },

    #' Initialize SQLite connection
    init_sqlite = function() {
      flags <- if (private$read_only) RSQLite::SQLITE_RO else RSQLite::SQLITE_RW
      private$con <- DBI::dbConnect(RSQLite::SQLite(), private$path, flags = flags)

      # Load metadata from SQLite
      books <- DBI::dbReadTable(private$con, "books")
      if (nrow(books) > 0) {
        private$book_guid <- books$guid[1]
        private$root_account_guid <- books$root_account_guid[1]
      }

      # Get default currency from root account
      private$load_default_currency_sqlite()

      # Cache XML tables as empty (will use lazy dbplyr)
      private$cache <- list(
        accounts = dplyr::collect(dplyr::tbl(private$con, "accounts")),
        transactions = dplyr::collect(dplyr::tbl(private$con, "transactions")),
        splits = dplyr::collect(dplyr::tbl(private$con, "splits")),
        commodities = dplyr::collect(dplyr::tbl(private$con, "commodities")),
        prices = if ("prices" %in% DBI::dbListTables(private$con)) {
          dplyr::collect(dplyr::tbl(private$con, "prices"))
        } else {
          tibble::tibble(
            guid = character(), commodity_guid = character(),
            currency_guid = character(), date = as.POSIXct(character()),
            source = character(), value_num = integer(), value_denom = integer()
          )
        }
      )
    },

    #' Initialize from XML file
    init_xml = function() {
      parsed <- parse_gnucash_xml(private$path)

      private$book_guid <- parsed$book_guid
      private$cache <- list(
        accounts = parsed$accounts,
        transactions = parsed$transactions,
        splits = parsed$splits,
        commodities = parsed$commodities,
        prices = parsed$prices
      )

      # Find root account and default currency
      root_accounts <- parsed$accounts |>
        dplyr::filter(account_type == "ROOT" | is.na(parent_guid))

      if (nrow(root_accounts) > 0) {
        private$root_account_guid <- root_accounts$guid[1]
      }

      # Get default currency (usually USD)
      currencies <- parsed$commodities |>
        dplyr::filter(namespace == "CURRENCY" | namespace == "ISO4217")

      if (nrow(currencies) > 0) {
        private$default_currency <- currencies$mnemonic[1]
      } else {
        private$default_currency <- "USD"
      }
    },

    #' Load default currency for SQLite format
    load_default_currency_sqlite = function() {
      if (!is.null(private$root_account_guid)) {
        root <- dplyr::tbl(private$con, "accounts") |>
          dplyr::filter(guid == !!private$root_account_guid) |>
          dplyr::collect()

        if (nrow(root) > 0 && !is.na(root$commodity_guid[1])) {
          currency <- dplyr::tbl(private$con, "commodities") |>
            dplyr::filter(guid == !!root$commodity_guid[1]) |>
            dplyr::collect()
          private$default_currency <- currency$mnemonic[1]
        }
      }

      if (is.null(private$default_currency)) {
        private$default_currency <- "USD"
      }
    }
  )
)


#' Read a GnuCash File
#'
#' Open a GnuCash file for reading. Automatically detects SQLite vs XML format.
#'
#' @param path Path to GnuCash file (.gnucash or .sqlite)
#' @param read_only Open in read-only mode (default TRUE)
#' @return A GnuCashDB object
#' @export
#' @examples
#' \dontrun{
#' gc <- read_gnucash("path/to/books.gnucash")
#' gc$accounts()
#' gc$close()
#' }
read_gnucash <- function(path, read_only = TRUE) {

  GnuCashDB$new(path, read_only = read_only)
}


#' Create a New GnuCash SQLite Database
#'
#' Create a new empty GnuCash file in SQLite format.
#' Initializes with minimal required structure (book, root account, currency).
#'
#' @param path Path for the new GnuCash file
#' @param currency Default currency (default: "USD")
#' @param overwrite If TRUE, overwrite existing file (default: FALSE)
#' @return A GnuCashDB object opened for writing
#' @export
#' @examples
#' \dontrun{
#' gc <- create_gnucash("path/to/new.gnucash")
#' create_account(gc, "Assets", "ASSET")
#' gc$close()
#' }
create_gnucash <- function(path, currency = "USD", overwrite = FALSE) {
  if (file.exists(path) && !overwrite) {
    rlang::abort(paste("File already exists:", path, "- use overwrite = TRUE to replace"))
  }

  if (file.exists(path) && overwrite) {
    file.remove(path)
  }

  # Create new SQLite database
  con <- DBI::dbConnect(RSQLite::SQLite(), path)

  tryCatch({
    # Create GnuCash schema
    create_gnucash_schema(con)

    # Generate GUIDs
    book_guid <- generate_guid()
    root_guid <- generate_guid()
    currency_guid <- generate_guid()

    # Insert book record
    DBI::dbExecute(con, "
      INSERT INTO books (guid, root_account_guid, root_template_guid)
      VALUES (?, ?, NULL)
    ", params = list(book_guid, root_guid))

    # Insert currency commodity
    DBI::dbExecute(con, "
      INSERT INTO commodities (guid, namespace, mnemonic, fullname, cusip, fraction, quote_flag, quote_source, quote_tz)
      VALUES (?, 'CURRENCY', ?, ?, NULL, 100, 0, 'currency', NULL)
    ", params = list(currency_guid, currency, currency))

    # Insert root account
    DBI::dbExecute(con, "
      INSERT INTO accounts (guid, name, account_type, commodity_guid, commodity_scu, non_std_scu, parent_guid, code, description, hidden, placeholder)
      VALUES (?, 'Root Account', 'ROOT', ?, 100, 0, NULL, '', '', 0, 0)
    ", params = list(root_guid, currency_guid))

    DBI::dbDisconnect(con)

    # Return opened GnuCashDB
    GnuCashDB$new(path, read_only = FALSE)

  }, error = function(e) {
    try(DBI::dbDisconnect(con), silent = TRUE)
    try(file.remove(path), silent = TRUE)
    rlang::abort(paste("Failed to create GnuCash database:", conditionMessage(e)))
  })
}


#' Create GnuCash SQLite Schema
#'
#' Create the required tables for a GnuCash SQLite database.
#'
#' @param con DBI connection
#' @noRd
create_gnucash_schema <- function(con) {
  # Books table
  DBI::dbExecute(con, "
    CREATE TABLE books (
      guid TEXT PRIMARY KEY NOT NULL,
      root_account_guid TEXT NOT NULL,
      root_template_guid TEXT
    )
  ")

  # Commodities table
  DBI::dbExecute(con, "
    CREATE TABLE commodities (
      guid TEXT PRIMARY KEY NOT NULL,
      namespace TEXT NOT NULL,
      mnemonic TEXT NOT NULL,
      fullname TEXT,
      cusip TEXT,
      fraction INTEGER NOT NULL,
      quote_flag INTEGER NOT NULL,
      quote_source TEXT,
      quote_tz TEXT
    )
  ")

  # Accounts table
  DBI::dbExecute(con, "
    CREATE TABLE accounts (
      guid TEXT PRIMARY KEY NOT NULL,
      name TEXT NOT NULL,
      account_type TEXT NOT NULL,
      commodity_guid TEXT,
      commodity_scu INTEGER NOT NULL,
      non_std_scu INTEGER NOT NULL,
      parent_guid TEXT,
      code TEXT,
      description TEXT,
      hidden INTEGER,
      placeholder INTEGER
    )
  ")

  # Transactions table
  DBI::dbExecute(con, "
    CREATE TABLE transactions (
      guid TEXT PRIMARY KEY NOT NULL,
      currency_guid TEXT NOT NULL,
      num TEXT,
      post_date TEXT,
      enter_date TEXT,
      description TEXT
    )
  ")

  # Splits table
  DBI::dbExecute(con, "
    CREATE TABLE splits (
      guid TEXT PRIMARY KEY NOT NULL,
      tx_guid TEXT NOT NULL,
      account_guid TEXT NOT NULL,
      memo TEXT,
      action TEXT,
      reconcile_state TEXT,
      reconcile_date TEXT,
      value_num INTEGER NOT NULL,
      value_denom INTEGER NOT NULL,
      quantity_num INTEGER NOT NULL,
      quantity_denom INTEGER NOT NULL,
      lot_guid TEXT,
      FOREIGN KEY (tx_guid) REFERENCES transactions(guid),
      FOREIGN KEY (account_guid) REFERENCES accounts(guid)
    )
  ")

  # Prices table (optional but useful)
  DBI::dbExecute(con, "
    CREATE TABLE prices (
      guid TEXT PRIMARY KEY NOT NULL,
      commodity_guid TEXT NOT NULL,
      currency_guid TEXT NOT NULL,
      date TEXT,
      source TEXT,
      type TEXT,
      value_num INTEGER NOT NULL,
      value_denom INTEGER NOT NULL
    )
  ")

  # Create indexes for performance
  DBI::dbExecute(con, "CREATE INDEX idx_accounts_parent ON accounts(parent_guid)")
  DBI::dbExecute(con, "CREATE INDEX idx_splits_tx ON splits(tx_guid)")
  DBI::dbExecute(con, "CREATE INDEX idx_splits_account ON splits(account_guid)")
  DBI::dbExecute(con, "CREATE INDEX idx_transactions_date ON transactions(post_date)")
}
