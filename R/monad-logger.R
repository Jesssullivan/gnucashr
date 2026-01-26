#' Logger Monad for Audit Trails
#'
#' Monadic wrapper for financial operations that captures detailed
#' audit logs of all operations performed.
#'
#' @name monad-logger
NULL

#' Create Logged Operation Result
#'
#' Wrap a value with an audit log entry.
#'
#' @param value The result value
#' @param log_entries Character vector of log entries
#' @return A logged result object
#' @export
logged <- function(value, log_entries = character()) {
  structure(
    list(
      value = value,
      log = log_entries
    ),
    class = "logged"
  )
}

#' Get Value from Logged Result
#'
#' Extract the value from a logged operation.
#'
#' @param x A logged object
#' @return The unwrapped value
#' @export
logged_value <- function(x) {
  if (!inherits(x, "logged")) {
    return(x)
  }
  x$value
}

#' Get Log from Logged Result
#'
#' Extract the audit log from a logged operation.
#'
#' @param x A logged object
#' @return Character vector of log entries
#' @export
logged_log <- function(x) {
  if (!inherits(x, "logged")) {
    return(character())
  }
  x$log
}

#' Append to Log
#'
#' Add a new entry to the log.
#'
#' @param x A logged object
#' @param entry New log entry string
#' @return Updated logged object
#' @export
log_append <- function(x, entry) {
  if (!inherits(x, "logged")) {
    x <- logged(x)
  }

  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  formatted_entry <- sprintf("[%s] %s", timestamp, entry)

  logged(x$value, c(x$log, formatted_entry))
}

#' Bind Logged Operations (Monadic Bind)
#'
#' Chain logged operations, accumulating logs.
#'
#' @param x A logged object
#' @param fn Function that takes a value and returns a logged object
#' @return New logged object with combined logs
#' @export
log_bind <- function(x, fn) {
  if (!inherits(x, "logged")) {
    x <- logged(x)
  }

  result <- fn(x$value)

  if (!inherits(result, "logged")) {
    result <- logged(result)
  }

  logged(result$value, c(x$log, result$log))
}

#' Map Over Logged Value
#'
#' Apply a function to the value without modifying the log.
#'
#' @param x A logged object
#' @param fn Function to apply to the value
#' @return Logged object with transformed value
#' @export
log_map <- function(x, fn) {
  if (!inherits(x, "logged")) {
    x <- logged(x)
  }

  logged(fn(x$value), x$log)
}

#' Print Logged Object
#'
#' @param x A logged object
#' @param ... Ignored
#' @export
print.logged <- function(x, ...) {
  cat("<Logged Result>\n")
  cat("Value type:", class(x$value)[1], "\n")
  cat("Log entries:", length(x$log), "\n")
  if (length(x$log) > 0) {
    cat("\nRecent log entries:\n")
    recent <- tail(x$log, 5)
    for (entry in recent) {
      cat("  ", entry, "\n")
    }
    if (length(x$log) > 5) {
      cat("  ... (", length(x$log) - 5, " more entries)\n")
    }
  }
  invisible(x)
}

# Financial Operation Wrappers ----

#' Audited Trial Balance
#'
#' Generate trial balance with full audit logging.
#'
#' @param gc GnuCashDB object
#' @param as_of Date for balance
#' @return Logged trial balance result
#' @export
audited_trial_balance <- function(gc, as_of = Sys.Date()) {
  as_of <- as.Date(as_of)
  meta <- gc$metadata()

  result <- logged(NULL) |>
    log_append(sprintf("Starting trial balance for %s", meta$path)) |>
    log_append(sprintf("As of date: %s", as_of)) |>
    log_append(sprintf("Book format: %s", meta$format))

  tb <- trial_balance(gc, as_of)

  result <- result |>
    log_append(sprintf("Processed %d accounts", nrow(tb))) |>
    log_append(sprintf("Total debits: %.2f", sum(tb$debit, na.rm = TRUE))) |>
    log_append(sprintf("Total credits: %.2f", sum(tb$credit, na.rm = TRUE)))

  # Check balance
  diff <- abs(sum(tb$debit, na.rm = TRUE) - sum(tb$credit, na.rm = TRUE))
  if (diff < 0.01) {
    result <- log_append(result, "Trial balance is in balance")
  } else {
    result <- log_append(result, sprintf("WARNING: Trial balance out of balance by %.2f", diff))
  }

  logged(tb, result$log)
}

#' Audited Consolidation
#'
#' Perform multi-book consolidation with audit logging.
#'
#' @param collection BookCollection object
#' @param as_of Date for balances
#' @param eliminate_ic Apply IC eliminations
#' @return Logged consolidated trial balance
#' @export
audited_consolidation <- function(collection, as_of = Sys.Date(), eliminate_ic = TRUE) {
  as_of <- as.Date(as_of)
  books <- collection$list_books()

  result <- logged(NULL) |>
    log_append("Starting consolidation") |>
    log_append(sprintf("Number of books: %d", nrow(books))) |>
    log_append(sprintf("Books: %s", paste(books$name, collapse = ", "))) |>
    log_append(sprintf("IC elimination: %s", if (eliminate_ic) "enabled" else "disabled"))

  # Get consolidated trial balance
  tb <- collection$consolidated_trial_balance(as_of, eliminate_ic)

  result <- result |>
    log_append(sprintf("Consolidated %d accounts", nrow(tb))) |>
    log_append(sprintf("Total assets: %.2f",
                       sum(tb$balance[tb$type %in% c("ASSET", "BANK", "CASH")], na.rm = TRUE))) |>
    log_append("Consolidation complete")

  logged(tb, result$log)
}

#' Write Audit Log to File
#'
#' Save the audit log to a file for compliance/review.
#'
#' @param x A logged object
#' @param path File path for the log
#' @param append Append to existing file (default FALSE)
#' @return Invisibly returns the logged object
#' @export
write_audit_log <- function(x, path, append = FALSE) {
  if (!inherits(x, "logged")) {
    rlang::abort("Expected a logged object")
  }

  header <- sprintf("\n=== Audit Log - %s ===\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  content <- paste(x$log, collapse = "\n")
  footer <- sprintf("\n=== End Log (%d entries) ===\n", length(x$log))

  writeLines(
    c(if (!append) "" else character(), header, content, footer),
    con = path,
    sep = ""
  )

  invisible(x)
}
