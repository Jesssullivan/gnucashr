# helper-fixtures.R - Test fixture management for gnucashr
#
# Provides utilities for working with pre-built test fixtures.
# Fixtures are static test data files stored in tests/testthat/fixtures/

# =============================================================================
# Fixture Path Helpers
# =============================================================================

#' Get Path to Test Fixture
#'
#' Constructs the path to a test fixture file. Fixtures are stored in
#' tests/testthat/fixtures/ relative to the package root.
#'
#' @param ... Path components to append to fixtures directory
#' @return Absolute path to the fixture file
#' @examples
#' \dontrun{
#' fixture_path("databases", "minimal.gnucash")
#' fixture_path("xml", "sample.gnucash")
#' }
fixture_path <- function(...) {
  # During tests, testthat sets the working directory
  # Use testthat::test_path for reliable fixture location

  if (requireNamespace("testthat", quietly = TRUE)) {
    testthat::test_path("fixtures", ...)
  } else {
    # Fallback for interactive use
    base_path <- system.file("tests", "testthat", "fixtures",
                             package = "gnucashr")
    if (base_path == "") {
      # Package not installed, try relative path
      base_path <- file.path("tests", "testthat", "fixtures")
    }
    file.path(base_path, ...)
  }
}


#' Check If Fixture Exists
#'
#' @param ... Path components (same as fixture_path)
#' @return Logical indicating if fixture exists
fixture_exists <- function(...) {
  file.exists(fixture_path(...))
}


# =============================================================================
# Database Fixture Loaders
# =============================================================================

#' Load Test Database Fixture
#'
#' Loads a GnuCash database fixture for testing. Creates a temporary copy
#' to avoid modifying the original fixture.
#'
#' @param name Name of the database fixture (without path)
#' @param read_only If TRUE (default), open in read-only mode
#' @param copy If TRUE (default), work with a temporary copy
#' @return A GnuCashDB object, or NULL if fixture not found
#' @examples
#' \dontrun{
#' gc <- load_test_db("minimal.gnucash")
#' gc <- load_test_db("with-accounts.gnucash", read_only = FALSE, copy = TRUE)
#' }
load_test_db <- function(name, read_only = TRUE, copy = TRUE) {
  fixture_file <- fixture_path("databases", name)

  if (!file.exists(fixture_file)) {
    warning(sprintf("Fixture database not found: %s", fixture_file))
    return(NULL)
  }

  if (copy) {
    # Create a temporary copy
    temp_file <- tempfile(fileext = ".gnucash")
    file.copy(fixture_file, temp_file)

    # Store original path for reference
    gc <- read_gnucash(temp_file, read_only = read_only)
    attr(gc, "fixture_source") <- fixture_file
    attr(gc, "is_copy") <- TRUE

    gc
  } else {
    read_gnucash(fixture_file, read_only = read_only)
  }
}


#' Load Test Database with Cleanup
#'
#' Like load_test_db but ensures cleanup using withr.
#' The database connection is automatically closed when the test completes.
#'
#' @param name Name of the database fixture
#' @param read_only If TRUE, open in read-only mode
#' @param envir Environment for cleanup registration
#' @return A GnuCashDB object
load_test_db_scoped <- function(name, read_only = TRUE, envir = parent.frame()) {
  gc <- load_test_db(name, read_only = read_only, copy = TRUE)

  if (!is.null(gc)) {
    withr::defer({
      if (gc$is_connected()) {
        gc$close()
      }
      # Clean up temp file if it exists
      if (isTRUE(attr(gc, "is_copy"))) {
        temp_path <- gc$metadata()$path
        if (file.exists(temp_path)) {
          unlink(temp_path)
        }
      }
    }, envir = envir)
  }

  gc
}


# =============================================================================
# Fixture Skip Helpers
# =============================================================================

#' Skip Test If Fixture Not Available
#'
#' @param ... Path components to fixture
skip_if_no_fixture <- function(...) {
  if (!fixture_exists(...)) {
    testthat::skip(sprintf("Fixture not available: %s", file.path(...)))
  }
  invisible(TRUE)
}


#' Skip Test If Database Fixture Not Available
#'
#' @param name Database fixture name
skip_if_no_db_fixture <- function(name) {
  skip_if_no_fixture("databases", name)
}


# =============================================================================
# Fixture Validation
# =============================================================================

#' Validate Database Fixture Integrity
#'
#' Checks that a database fixture has the expected structure.
#'
#' @param name Database fixture name
#' @return List with validation results
#' @noRd
validate_db_fixture <- function(name) {
  fixture_file <- fixture_path("databases", name)

  if (!file.exists(fixture_file)) {
    return(list(valid = FALSE, error = "File not found"))
  }

  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), fixture_file, flags = RSQLite::SQLITE_RO)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    tables <- DBI::dbListTables(con)
    required <- c("books", "accounts", "commodities", "transactions", "splits")
    missing <- setdiff(required, tables)

    if (length(missing) > 0) {
      return(list(
        valid = FALSE,
        error = sprintf("Missing tables: %s", paste(missing, collapse = ", "))
      ))
    }

    # Check for at least one book record
    books <- DBI::dbReadTable(con, "books")
    if (nrow(books) == 0) {
      return(list(valid = FALSE, error = "No book record found"))
    }

    # Check for root account
    accounts <- DBI::dbReadTable(con, "accounts")
    root <- accounts[accounts$account_type == "ROOT", ]
    if (nrow(root) == 0) {
      return(list(valid = FALSE, error = "No root account found"))
    }

    list(
      valid = TRUE,
      tables = tables,
      n_accounts = nrow(accounts),
      n_transactions = nrow(DBI::dbReadTable(con, "transactions"))
    )
  }, error = function(e) {
    list(valid = FALSE, error = conditionMessage(e))
  })
}


#' List Available Database Fixtures
#'
#' @return Character vector of available fixture names
list_db_fixtures <- function() {
  db_dir <- fixture_path("databases")
  if (!dir.exists(db_dir)) {
    return(character(0))
  }

  files <- list.files(db_dir, pattern = "\\.gnucash$", full.names = FALSE)
  files
}
