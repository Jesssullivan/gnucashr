# helper-common.R - Shared test utilities for gnucashr
#
# This file is automatically loaded by testthat before tests run.
# See: https://testthat.r-lib.org/articles/test-fixtures.html

# =============================================================================
# Database Creation Helpers
# =============================================================================

#' Create a Minimal Test GnuCash SQLite Database
#'
#' Creates a minimal but valid GnuCash SQLite database with:
#' - Required tables (books, commodities, accounts, transactions, splits)
#' - Root account with USD currency
#' - Basic account hierarchy (Assets, Expenses, Checking)
#'
#' @param path File path for the database. If NULL, uses tempfile.
#' @return Named list with path and GUIDs for created entities
#' @examples
#' \dontrun{
#' db_info <- create_test_gnucash_db()
#' gc <- read_gnucash(db_info$path, read_only = FALSE)
#' }
create_test_gnucash_db <- function(path = NULL) {
  if (is.null(path)) {
    path <- tempfile(fileext = ".gnucash")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Create minimal schema
  DBI::dbExecute(con, "
    CREATE TABLE books (
      guid TEXT PRIMARY KEY,
      root_account_guid TEXT,
      root_template_guid TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE commodities (
      guid TEXT PRIMARY KEY,
      namespace TEXT,
      mnemonic TEXT,
      fullname TEXT,
      cusip TEXT,
      fraction INTEGER,
      quote_flag INTEGER,
      quote_source TEXT,
      quote_tz TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE accounts (
      guid TEXT PRIMARY KEY,
      name TEXT,
      account_type TEXT,
      commodity_guid TEXT,
      commodity_scu INTEGER,
      non_std_scu INTEGER,
      parent_guid TEXT,
      code TEXT,
      description TEXT,
      hidden INTEGER,
      placeholder INTEGER
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE transactions (
      guid TEXT PRIMARY KEY,
      currency_guid TEXT,
      num TEXT,
      post_date TEXT,
      enter_date TEXT,
      description TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE splits (
      guid TEXT PRIMARY KEY,
      tx_guid TEXT,
      account_guid TEXT,
      memo TEXT,
      action TEXT,
      reconcile_state TEXT,
      reconcile_date TEXT,
      value_num INTEGER,
      value_denom INTEGER,
      quantity_num INTEGER,
      quantity_denom INTEGER,
      lot_guid TEXT
    )
  ")

  # Create prices table
  DBI::dbExecute(con, "
    CREATE TABLE prices (
      guid TEXT PRIMARY KEY,
      commodity_guid TEXT,
      currency_guid TEXT,
      date TEXT,
      source TEXT,
      type TEXT,
      value_num INTEGER,
      value_denom INTEGER
    )
  ")

  # Generate deterministic GUIDs for test reproducibility
  root_guid <- "00000000000000000000000000000001"
  book_guid <- "00000000000000000000000000000002"
  usd_guid <- "00000000000000000000000000000003"
  assets_guid <- "00000000000000000000000000000004"
  expenses_guid <- "00000000000000000000000000000005"
  checking_guid <- "00000000000000000000000000000006"

  # Insert test data

  DBI::dbExecute(con, "INSERT INTO books VALUES (?, ?, NULL)",
                 params = list(book_guid, root_guid))

  DBI::dbExecute(con, "INSERT INTO commodities VALUES (?, 'CURRENCY', 'USD', 'US Dollar', NULL, 100, 0, NULL, NULL)",
                 params = list(usd_guid))

  DBI::dbExecute(con, "INSERT INTO accounts VALUES (?, 'Root Account', 'ROOT', ?, 100, 0, NULL, '', '', 0, 0)",
                 params = list(root_guid, usd_guid))

  DBI::dbExecute(con, "INSERT INTO accounts VALUES (?, 'Assets', 'ASSET', ?, 100, 0, ?, '', '', 0, 1)",
                 params = list(assets_guid, usd_guid, root_guid))

  DBI::dbExecute(con, "INSERT INTO accounts VALUES (?, 'Expenses', 'EXPENSE', ?, 100, 0, ?, '', '', 0, 1)",
                 params = list(expenses_guid, usd_guid, root_guid))

  DBI::dbExecute(con, "INSERT INTO accounts VALUES (?, 'Checking', 'BANK', ?, 100, 0, ?, '', '', 0, 0)",
                 params = list(checking_guid, usd_guid, assets_guid))

  list(
    path = path,
    root_guid = root_guid,
    book_guid = book_guid,
    usd_guid = usd_guid,
    assets_guid = assets_guid,
    expenses_guid = expenses_guid,
    checking_guid = checking_guid
  )
}


#' Execute Code with a Temporary GnuCash Database
#'
#' Creates a temporary GnuCash database, executes the provided code,
#' and ensures cleanup regardless of success or failure.
#'
#' @param code Code to execute with the temporary database
#' @param with_accounts If TRUE, create standard account hierarchy
#' @return Result of the code execution
#' @examples
#' \dontrun{
#' with_temp_gnucash({
#'   gc <- read_gnucash(.db_info$path, read_only = FALSE)
#'   gc$accounts()
#' })
#' }
with_temp_gnucash <- function(code, with_accounts = TRUE) {
  db_info <- create_test_gnucash_db()


  withr::defer({
    if (file.exists(db_info$path)) {
      unlink(db_info$path)
    }
    # Also clean up any backup directories
    backup_dir <- file.path(dirname(db_info$path), ".gnucash-backups")
    if (dir.exists(backup_dir)) {
      unlink(backup_dir, recursive = TRUE)
    }
  })

  # Make db_info available in the calling environment
  env <- parent.frame()
  env$.db_info <- db_info

  eval(substitute(code), envir = env)
}


# =============================================================================
# Skip Helpers
# =============================================================================

#' Skip Test if Rcpp Functions Are Not Available
#'
#' Some Rcpp functions may not be available if the package was not compiled
#' with C++ support. This helper skips tests gracefully.
#'
#' @param fn_name Name of the Rcpp function to check (optional)
skip_if_no_rcpp <- function(fn_name = "generate_guid") {
  skip_if_not(
    exists(fn_name, envir = asNamespace("gnucashr"), inherits = FALSE),
    message = sprintf("Rcpp function '%s' not available (C++ compilation required)", fn_name)
  )
}


#' Skip Test if R6 Class Is Not Available
#'
#' @param class_name Name of the R6 class to check
skip_if_no_class <- function(class_name) {
  cls <- tryCatch(
    get(class_name, envir = asNamespace("gnucashr")),
    error = function(e) NULL
  )
  skip_if_not(
    !is.null(cls) && inherits(cls, "R6ClassGenerator"),
    message = sprintf("R6 class '%s' not available", class_name)
  )
}


# =============================================================================
# Custom Expectations
# =============================================================================
#' Expect a Balanced Transaction
#'
#' Custom expectation that verifies splits in a transaction sum to zero
#' (double-entry bookkeeping principle).
#'
#' @param splits A tibble or data frame with value_num and value_denom columns
#' @param tolerance Maximum allowed imbalance (default 0.001)
#' @return Invisible TRUE if balanced, fails otherwise
#' @examples
#' \dontrun{
#' splits <- tibble::tibble(
#'   value_num = c(10000L, -10000L),
#'   value_denom = c(100L, 100L)
#' )
#' expect_balanced_transaction(splits)
#' }
expect_balanced_transaction <- function(splits, tolerance = 0.001) {
  act <- testthat::quasi_label(rlang::enquo(splits), arg = "splits")

  # Calculate total using fraction arithmetic
  if (!all(c("value_num", "value_denom") %in% names(act$val))) {
    testthat::fail("splits must have 'value_num' and 'value_denom' columns")
  }

  total <- sum(act$val$value_num / act$val$value_denom)

  if (abs(total) > tolerance) {
    testthat::fail(sprintf(
      "Transaction is not balanced. Total: %.4f (tolerance: %.4f)",
      total, tolerance
    ))
  }

  testthat::succeed()
  invisible(TRUE)
}


#' Expect a Valid GnuCash GUID
#'
#' Custom expectation that verifies a string is a valid 32-character hex GUID.
#'
#' @param guid Character string to validate
#' @return Invisible TRUE if valid, fails otherwise
expect_valid_guid <- function(guid) {
  act <- testthat::quasi_label(rlang::enquo(guid), arg = "guid")

  if (!is.character(act$val) || length(act$val) != 1) {
    testthat::fail("GUID must be a single character string")
  }

  if (!grepl("^[a-f0-9]{32}$", act$val, ignore.case = TRUE)) {
    testthat::fail(sprintf(
      "'%s' is not a valid GnuCash GUID (expected 32 hex characters)",
      act$val
    ))
  }

  testthat::succeed()
  invisible(TRUE)
}


#' Expect a Result to Be Ok
#'
#' Custom expectation for the Result monad.
#'
#' @param result A Result object (from ok() or err())
expect_result_ok <- function(result) {
  act <- testthat::quasi_label(rlang::enquo(result), arg = "result")

  if (!is_ok(act$val)) {
    msg <- tryCatch(unwrap_err(act$val), error = function(e) "unknown error")
    testthat::fail(sprintf("Expected ok Result, got error: %s", msg))
  }

  testthat::succeed()
  invisible(TRUE)
}


#' Expect a Result to Be Err
#'
#' Custom expectation for the Result monad.
#'
#' @param result A Result object (from ok() or err())
#' @param pattern Optional regex pattern to match error message
expect_result_err <- function(result, pattern = NULL)
{
  act <- testthat::quasi_label(rlang::enquo(result), arg = "result")

  if (!is_err(act$val)) {
    testthat::fail("Expected err Result, got ok")
  }

  if (!is.null(pattern)) {
    msg <- unwrap_err(act$val)
    if (!grepl(pattern, msg)) {
      testthat::fail(sprintf(
        "Error message '%s' does not match pattern '%s'",
        msg, pattern
      ))
    }
  }

  testthat::succeed()
  invisible(TRUE)
}


# =============================================================================
# Test Data Generators
# =============================================================================

#' Generate a Test Transaction with Balanced Splits
#'
#' Creates a balanced two-split transaction for testing.
#'
#' @param from_account_guid Source account GUID
#' @param to_account_guid Destination account GUID
#' @param amount Transaction amount in cents (positive integer)
#' @param description Transaction description
#' @param date Transaction date
#' @return List with transaction and splits data
generate_test_transaction <- function(
    from_account_guid = "00000000000000000000000000000006",
    to_account_guid = "00000000000000000000000000000005",
    amount = 10000L,  # $100.00 in cents
    description = "Test transaction",
    date = Sys.time()
) {
  tx_guid <- if (exists("generate_guid")) generate_guid() else sprintf("%032x", sample.int(.Machine$integer.max, 1))
  split1_guid <- if (exists("generate_guid")) generate_guid() else sprintf("%032x", sample.int(.Machine$integer.max, 1))
  split2_guid <- if (exists("generate_guid")) generate_guid() else sprintf("%032x", sample.int(.Machine$integer.max, 1))

  list(
    transaction = tibble::tibble(
      guid = tx_guid,
      currency_guid = "00000000000000000000000000000003",
      num = "",
      post_date = format(date, "%Y-%m-%d %H:%M:%S"),
      enter_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      description = description
    ),
    splits = tibble::tibble(
      guid = c(split1_guid, split2_guid),
      tx_guid = c(tx_guid, tx_guid),
      account_guid = c(from_account_guid, to_account_guid),
      memo = c("", ""),
      action = c("", ""),
      reconcile_state = c("n", "n"),
      reconcile_date = c(NA_character_, NA_character_),
      value_num = c(-amount, amount),
      value_denom = c(100L, 100L),
      quantity_num = c(-amount, amount),
      quantity_denom = c(100L, 100L),
      lot_guid = c(NA_character_, NA_character_)
    )
  )
}
