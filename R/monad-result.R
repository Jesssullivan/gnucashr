#' Result Monad for Safe Error Handling
#'
#' Monadic wrapper for operations that may fail, providing
#' safe error handling without exceptions.
#'
#' @name monad-result
NULL

#' Create Success Result
#'
#' Wrap a successful value.
#'
#' @param value The successful result value
#' @return A result object with ok status
#' @export
ok <- function(value) {
  structure(
    list(
      ok = TRUE,
      value = value,
      error = NULL
    ),
    class = c("result", "ok_result")
  )
}

#' Create Error Result
#'
#' Wrap an error condition.
#'
#' @param error Error message or condition
#' @return A result object with error status
#' @export
err <- function(error) {
  structure(
    list(
      ok = FALSE,
      value = NULL,
      error = error
    ),
    class = c("result", "err_result")
  )
}

#' Check if Result is Ok
#'
#' @param x A result object
#' @return Logical indicating success
#' @export
is_ok <- function(x) {
  inherits(x, "ok_result")
}

#' Check if Result is Error
#'
#' @param x A result object
#' @return Logical indicating error
#' @export
is_err <- function(x) {
  inherits(x, "err_result")
}

#' Unwrap Result Value
#'
#' Get the value from an Ok result, or abort for Err.
#'
#' @param x A result object
#' @param default Optional default value if error
#' @return The unwrapped value
#' @export
unwrap <- function(x, default = NULL) {
  if (!inherits(x, "result")) {
    return(x)
  }

  if (is_ok(x)) {
    return(x$value)
  }

  if (!is.null(default)) {
    return(default)
  }

  rlang::abort(
    message = paste("Attempted to unwrap an error result:", x$error),
    class = "unwrap_error"
  )
}

#' Unwrap Error
#'
#' Get the error from an Err result, or abort for Ok.
#'
#' @param x A result object
#' @return The error value
#' @export
unwrap_err <- function(x) {
  if (!inherits(x, "result")) {
    rlang::abort("Not a result object")
  }

  if (is_err(x)) {
    return(x$error)
  }

  rlang::abort("Attempted to unwrap_err an Ok result")
}

#' Map Over Result (Functor)
#'
#' Apply a function to the value if Ok, pass through if Err.
#'
#' @param x A result object
#' @param fn Function to apply
#' @return New result with transformed value
#' @export
result_map <- function(x, fn) {
  if (!inherits(x, "result")) {
    x <- ok(x)
  }

  if (is_ok(x)) {
    ok(fn(x$value))
  } else {
    x
  }
}

#' Map Over Error
#'
#' Apply a function to the error if Err, pass through if Ok.
#'
#' @param x A result object
#' @param fn Function to apply to error
#' @return New result with transformed error
#' @export
result_map_err <- function(x, fn) {
  if (!inherits(x, "result")) {
    return(ok(x))
  }

  if (is_err(x)) {
    err(fn(x$error))
  } else {
    x
  }
}

#' Bind Result (Monadic Bind)
#'
#' Chain result operations, short-circuiting on error.
#'
#' @param x A result object
#' @param fn Function that takes a value and returns a result
#' @return Result from fn or the original error
#' @export
result_bind <- function(x, fn) {
  if (!inherits(x, "result")) {
    x <- ok(x)
  }

  if (is_ok(x)) {
    result <- fn(x$value)
    if (!inherits(result, "result")) {
      result <- ok(result)
    }
    result
  } else {
    x
  }
}

#' Pattern Match on Result
#'
#' Handle both Ok and Err cases with separate functions.
#'
#' @param x A result object
#' @param ok_fn Function to call if Ok
#' @param err_fn Function to call if Err
#' @return Result of the matched function
#' @export
result_match <- function(x, ok_fn, err_fn) {
  if (!inherits(x, "result")) {
    return(ok_fn(x))
  }

  if (is_ok(x)) {
    ok_fn(x$value)
  } else {
    err_fn(x$error)
  }
}

#' Try Expression as Result
#'
#' Execute an expression and capture errors as Err result.
#'
#' @param expr Expression to evaluate
#' @return Result with value or error
#' @export
try_result <- function(expr) {
  tryCatch(
    {
      ok(expr)
    },
    error = function(e) {
      err(conditionMessage(e))
    }
  )
}

#' Print Result Object
#'
#' @param x A result object
#' @param ... Ignored
#' @export
print.result <- function(x, ...) {
  if (is_ok(x)) {
    cat("<Ok>\n")
    cat("Value type:", class(x$value)[1], "\n")
    if (is.data.frame(x$value)) {
      cat("Rows:", nrow(x$value), "\n")
    }
  } else {
    cat("<Err>\n")
    cat("Error:", x$error, "\n")
  }
  invisible(x)
}

# Safe Financial Operations ----

#' Safe Read GnuCash
#'
#' Open a GnuCash file with error handling.
#'
#' @param path Path to GnuCash file
#' @param read_only Read-only mode
#' @return Result with GnuCashDB or error
#' @export
safe_read_gnucash <- function(path, read_only = TRUE) {
  try_result(read_gnucash(path, read_only))
}

#' Safe Trial Balance
#'
#' Generate trial balance with error handling.
#'
#' @param gc GnuCashDB object
#' @param as_of Date for balance
#' @return Result with trial balance or error
#' @export
safe_trial_balance <- function(gc, as_of = Sys.Date()) {
  if (!inherits(gc, "GnuCashDB")) {
    return(err("Expected GnuCashDB object"))
  }

  if (!gc$is_connected()) {
    return(err("GnuCash database is not connected"))
  }

  try_result(trial_balance(gc, as_of))
}

#' Safe Consolidation
#'
#' Perform consolidation with error handling.
#'
#' @param collection BookCollection object
#' @param as_of Date for balances
#' @param eliminate_ic Apply IC eliminations
#' @return Result with consolidated data or error
#' @export
safe_consolidation <- function(collection, as_of = Sys.Date(), eliminate_ic = TRUE) {
  if (!inherits(collection, "BookCollection")) {
    return(err("Expected BookCollection object"))
  }

  books <- collection$list_books()
  if (nrow(books) == 0) {
    return(err("No books in collection"))
  }

  try_result(collection$consolidated_trial_balance(as_of, eliminate_ic))
}

#' Combine Results
#'
#' Combine multiple results, failing if any fail.
#'
#' @param ... Result objects to combine
#' @return Result with list of values or first error
#' @export
combine_results <- function(...) {
  results <- list(...)

  # Check for any errors
  errors <- purrr::keep(results, is_err)
  if (length(errors) > 0) {
    return(errors[[1]])
  }

  # All ok, collect values
  values <- purrr::map(results, unwrap)
  ok(values)
}
