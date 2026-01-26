#' Result Monad Integration for LazyForecast
#'
#' Safe error handling for forecast operations using the Result monad.
#' All errors are captured and returned as Err results instead of exceptions.
#'
#' @name forecast-result
NULL

#' Wrap LazyForecast with Result Monad
#'
#' Create a function that collects a LazyForecast with error handling.
#'
#' @param lf LazyForecast object
#' @return Function that returns Result when called
#'
#' @examples
#' \dontrun{
#' forecast <- forecast_expr(config) |>
#'   lf_monte_carlo(n = 1000)
#'
#' # Wrap for safe execution
#' safe_fn <- lazy_result(forecast)
#' result <- safe_fn()
#'
#' # Handle result
#' result_match(result,
#'   ok_fn = function(data) data$summary$p50,
#'   err_fn = function(e) warning(e)
#' )
#' }
#'
#' @export
lazy_result <- function(lf) {
  if (!inherits(lf, "LazyForecast")) {
    rlang::abort("lf must be a LazyForecast object")
  }

  function(parallel = TRUE) {
    try_result(lf$collect(parallel = parallel))
  }
}

#' Safe Collect with Result Monad
#'
#' Execute a LazyForecast with error handling.
#' Returns Ok(result) on success or Err(message) on failure.
#'
#' @param lf LazyForecast object
#' @param parallel Use parallel execution
#' @return Result object (Ok or Err)
#'
#' @examples
#' \dontrun{
#' forecast <- forecast_expr(config) |>
#'   lf_monte_carlo(n = 1000)
#'
#' result <- safe_collect(forecast)
#'
#' if (is_ok(result)) {
#'   data <- unwrap(result)
#'   print(data$summary)
#' } else {
#'   warning("Forecast failed: ", unwrap_err(result))
#' }
#' }
#'
#' @export
safe_collect <- function(lf, parallel = TRUE) {
  if (!inherits(lf, "LazyForecast")) {
    return(err("Expected LazyForecast object"))
  }

  try_result(lf$collect(parallel = parallel))
}

#' Safe Forecast Pipeline
#'
#' Build and execute a forecast pipeline with full error handling.
#' Each step is validated and errors are captured as Results.
#'
#' @param source Data source
#' @param ... Pipeline operations to apply
#' @param parallel Use parallel execution
#' @return Result with forecast output or error
#'
#' @examples
#' \dontrun{
#' result <- safe_forecast_pipeline(
#'   config,
#'   lf_monte_carlo(n = 1000),
#'   lf_grow(rate = 0.05, months = 12)
#' )
#' }
#'
#' @export
safe_forecast_pipeline <- function(source, ..., parallel = TRUE) {
  # Validate source
  source_result <- try_result(forecast_expr(source))
  if (is_err(source_result)) {
    return(source_result)
  }

  lf <- unwrap(source_result)

  # Apply operations
  ops <- list(...)
  for (op in ops) {
    op_result <- try_result(op(lf))
    if (is_err(op_result)) {
      return(op_result)
    }
    lf <- unwrap(op_result)
  }

  # Collect with error handling
  safe_collect(lf, parallel = parallel)
}

#' Map Result Over Forecasts
#'
#' Apply a function to forecast results if Ok, propagate Err.
#'
#' @param result Result object from safe_collect
#' @param fn Function to apply to the Ok value
#' @return New Result with transformed value
#'
#' @export
map_forecast_result <- function(result, fn) {
  result_map(result, fn)
}

#' Chain Forecast Results
#'
#' Chain forecast operations, short-circuiting on error.
#'
#' @param result Result object
#' @param fn Function that takes a value and returns a Result
#' @return Result from fn or the original error
#'
#' @export
bind_forecast_result <- function(result, fn) {
  result_bind(result, fn)
}

#' Extract Summary from Result
#'
#' Safely extract summary statistics from a Monte Carlo result.
#'
#' @param result Result from safe_collect of Monte Carlo forecast
#' @return Result with summary or error
#'
#' @export
extract_mc_summary <- function(result) {
  result_bind(result, function(data) {
    if (!is.list(data)) {
      return(err("Expected list result from Monte Carlo"))
    }
    if (!"summary" %in% names(data)) {
      return(err("No summary found in Monte Carlo result"))
    }
    ok(data$summary)
  })
}

#' Extract Quantile from Result
#'
#' Safely extract a specific quantile from Monte Carlo result.
#'
#' @param result Result from safe_collect
#' @param quantile Quantile name (e.g., "p50", "p95")
#' @param default Default value if extraction fails
#' @return Numeric quantile value or default
#'
#' @export
extract_quantile <- function(result, quantile = "p50", default = NA_real_) {
  if (is_err(result)) {
    return(default)
  }

  data <- unwrap(result)
  if (is.list(data) && "summary" %in% names(data)) {
    return(data$summary[[quantile]] %||% default)
  }

  default
}

#' Combine Forecast Results
#'
#' Combine multiple forecast results, failing if any fail.
#'
#' @param ... Result objects from safe_collect
#' @param names Optional names for results
#' @return Result with list of values or first error
#'
#' @export
combine_forecast_results <- function(..., names = NULL) {
  results <- list(...)

  if (!is.null(names) && length(names) == length(results)) {
    names(results) <- names
  }

  combine_results(...)
}

#' Safe Comparison of Forecasts
#'
#' Compare multiple forecasts with error handling.
#'
#' @param ... Named LazyForecast objects
#' @param parallel Use parallel execution
#' @return Result with comparison tibble or error
#'
#' @examples
#' \dontrun{
#' result <- safe_compare_forecasts(
#'   base = forecast_expr(config) |> lf_monte_carlo(n = 1000),
#'   optimistic = forecast_expr(config) |> lf_monte_carlo(n = 1000, growth_mean = 0.08)
#' )
#'
#' result_match(result,
#'   ok_fn = print,
#'   err_fn = function(e) warning("Comparison failed: ", e)
#' )
#' }
#'
#' @export
safe_compare_forecasts <- function(..., parallel = TRUE) {
  try_result(compare_forecasts(..., parallel = parallel))
}

#' Validate Forecast Parameters
#'
#' Validate Monte Carlo or sensitivity parameters before execution.
#'
#' @param n Number of simulations (must be positive)
#' @param months Number of months (must be positive)
#' @param growth_mean Mean growth rate
#' @param growth_sd Growth standard deviation (must be non-negative)
#' @return Result with validated params or error
#'
#' @export
validate_forecast_params <- function(n = 10000, months = 12,
                                     growth_mean = 0.05, growth_sd = 0.03) {
  errors <- character()

  if (n <= 0) {
    errors <- c(errors, "n must be positive")
  }
  if (months <= 0) {
    errors <- c(errors, "months must be positive")
  }
  if (growth_sd < 0) {
    errors <- c(errors, "growth_sd must be non-negative")
  }
  if (n > 1e7) {
    errors <- c(errors, "n exceeds maximum (10 million)")
  }

  if (length(errors) > 0) {
    return(err(paste(errors, collapse = "; ")))
  }

  ok(list(
    n = n,
    months = months,
    growth_mean = growth_mean,
    growth_sd = growth_sd
  ))
}

#' Safe Quick Monte Carlo
#'
#' Run quick_monte_carlo with error handling.
#'
#' @param source Data source
#' @param n Number of simulations
#' @param months Number of months
#' @param growth_mean Mean growth rate
#' @param growth_sd Growth standard deviation
#' @param seed Random seed
#' @return Result with Monte Carlo output or error
#'
#' @export
safe_quick_monte_carlo <- function(source, n = 10000, months = 12,
                                   growth_mean = 0.05, growth_sd = 0.03,
                                   seed = 42L) {
  # Validate parameters first
  param_result <- validate_forecast_params(n, months, growth_mean, growth_sd)
  if (is_err(param_result)) {
    return(param_result)
  }

  # Run Monte Carlo
  try_result(
    quick_monte_carlo(source, n, months, growth_mean, growth_sd, seed)
  )
}

#' Safe Quick Sensitivity
#'
#' Run quick_sensitivity with error handling.
#'
#' @param source Data source
#' @param growth_range Growth rate range
#' @param expense_range Expense ratio range
#' @param months Number of months
#' @return Result with sensitivity output or error
#'
#' @export
safe_quick_sensitivity <- function(source,
                                   growth_range = seq(-0.02, 0.08, by = 0.01),
                                   expense_range = seq(0.5, 0.9, by = 0.05),
                                   months = 12) {
  try_result(
    quick_sensitivity(source, growth_range, expense_range, months)
  )
}
