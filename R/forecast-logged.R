#' Logger Monad Integration for LazyForecast
#'
#' Audit trail logging for forecast operations using the Logger monad.
#' All operations are recorded with timestamps for compliance and debugging.
#'
#' @name forecast-logged
NULL

#' Create Audited Forecast
#'
#' Wrap a LazyForecast with audit logging.
#'
#' @param lf LazyForecast object
#' @param description Description of the forecast purpose
#' @return Logged object containing LazyForecast
#'
#' @examples
#' \dontrun{
#' forecast <- forecast_expr(config) |>
#'   lf_monte_carlo(n = 1000)
#'
#' audited <- audited_forecast(forecast, "Q1 2026 cash flow projection")
#' }
#'
#' @export
audited_forecast <- function(lf, description = "Financial forecast") {
  if (!inherits(lf, "LazyForecast")) {
    rlang::abort("lf must be a LazyForecast object")
  }

  meta <- lf$metadata()

  logged(lf) |>
    log_append(sprintf("Forecast initialized: %s", description)) |>
    log_append(sprintf("Source type: %s", meta$source_type)) |>
    log_append(sprintf("Operations: %d (%s)",
                       meta$n_operations,
                       paste(meta$operations, collapse = ", ")))
}

#' Audited Collect
#'
#' Execute a LazyForecast with full audit logging.
#' Records execution time, parameters, and result summary.
#'
#' @param audited_lf Logged object containing LazyForecast
#' @param parallel Use parallel execution
#' @return Logged object with materialized result
#'
#' @examples
#' \dontrun{
#' forecast <- forecast_expr(config) |>
#'   lf_monte_carlo(n = 1000)
#'
#' audited <- audited_forecast(forecast, "Monthly projection")
#' result <- audited_collect(audited)
#'
#' # Get result data
#' data <- logged_value(result)
#'
#' # Get audit log
#' log <- logged_log(result)
#' }
#'
#' @export
audited_collect <- function(audited_lf, parallel = TRUE) {
  if (!inherits(audited_lf, "logged")) {
    rlang::abort("audited_lf must be a logged object")
  }

  lf <- logged_value(audited_lf)
  if (!inherits(lf, "LazyForecast")) {
    rlang::abort("logged value must be a LazyForecast")
  }

  # Record start
  start_time <- Sys.time()
  result <- audited_lf |>
    log_append(sprintf("Execution started at %s", format(start_time))) |>
    log_append(sprintf("Parallel execution: %s", parallel))

  # Execute
  data <- lf$collect(parallel = parallel)

  # Record completion
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "secs")

  result <- result |>
    log_append(sprintf("Execution completed in %.3f seconds", duration))

  # Add result-specific logging
  if (is.list(data) && "summary" %in% names(data)) {
    # Monte Carlo result
    result <- result |>
      log_append(sprintf("Monte Carlo simulations: %d", data$params$n_sims %||% NA)) |>
      log_append(sprintf("Mean outcome: %.2f", data$summary$mean %||% NA)) |>
      log_append(sprintf("P50 (median): %.2f", data$summary$p50 %||% NA)) |>
      log_append(sprintf("P95 (upper): %.2f", data$summary$p95 %||% NA))
  } else if (is.list(data) && "outcomes" %in% names(data)) {
    # Sensitivity result
    outcomes <- data$outcomes
    result <- result |>
      log_append(sprintf("Sensitivity grid: %d x %d",
                         nrow(outcomes), ncol(outcomes))) |>
      log_append(sprintf("Outcome range: %.2f to %.2f",
                         min(outcomes), max(outcomes)))
  } else if (is.data.frame(data)) {
    # Data frame result
    result <- result |>
      log_append(sprintf("Result rows: %d", nrow(data))) |>
      log_append(sprintf("Result columns: %d", ncol(data)))
  }

  # Return logged result
  logged(data, logged_log(result))
}

#' Audited Monte Carlo
#'
#' Run Monte Carlo simulation with full audit logging.
#'
#' @param source Data source
#' @param n Number of simulations
#' @param months Number of months
#' @param growth_mean Mean growth rate
#' @param growth_sd Growth standard deviation
#' @param seed Random seed
#' @param description Audit description
#' @return Logged Monte Carlo result
#'
#' @examples
#' \dontrun{
#' result <- audited_monte_carlo(
#'   config,
#'   n = 10000,
#'   months = 12,
#'   description = "Annual cash flow projection for board meeting"
#' )
#'
#' # Get results
#' data <- logged_value(result)
#' print(data$summary)
#'
#' # Write audit log
#' write_audit_log(result, "forecast_audit.log")
#' }
#'
#' @export
audited_monte_carlo <- function(source, n = 10000, months = 12,
                                growth_mean = 0.05, growth_sd = 0.03,
                                seed = 42L,
                                description = "Monte Carlo simulation") {
  # Create forecast
  forecast <- forecast_expr(source) |>
    lf_monte_carlo(
      n = n,
      growth_mean = growth_mean,
      growth_sd = growth_sd,
      months = months,
      seed = seed
    )

  # Create audit trail
  audited <- logged(NULL) |>
    log_append(description) |>
    log_append(sprintf("Parameters: n=%d, months=%d, growth_mean=%.3f, growth_sd=%.3f, seed=%d",
                       n, months, growth_mean, growth_sd, seed))

  # Execute with timing
  start_time <- Sys.time()
  data <- forecast$collect()
  duration <- difftime(Sys.time(), start_time, units = "secs")

  # Complete audit trail
  audited <- audited |>
    log_append(sprintf("Execution completed in %.3f seconds", duration)) |>
    log_append(sprintf("Mean outcome: $%.2f", data$summary$mean)) |>
    log_append(sprintf("Median (P50): $%.2f", data$summary$p50)) |>
    log_append(sprintf("5th percentile: $%.2f", data$summary$p5)) |>
    log_append(sprintf("95th percentile: $%.2f", data$summary$p95))

  logged(data, logged_log(audited))
}

#' Audited Sensitivity Analysis
#'
#' Run sensitivity analysis with full audit logging.
#'
#' @param source Data source
#' @param growth_range Growth rate range
#' @param expense_range Expense ratio range
#' @param months Number of months
#' @param description Audit description
#' @return Logged sensitivity result
#'
#' @export
audited_sensitivity <- function(source,
                                growth_range = seq(-0.02, 0.08, by = 0.01),
                                expense_range = seq(0.5, 0.9, by = 0.05),
                                months = 12,
                                description = "Sensitivity analysis") {
  forecast <- forecast_expr(source) |>
    lf_sensitivity(
      growth_range = growth_range,
      expense_range = expense_range,
      months = months
    )

  audited <- logged(NULL) |>
    log_append(description) |>
    log_append(sprintf("Growth range: %.2f%% to %.2f%%",
                       min(growth_range) * 100, max(growth_range) * 100)) |>
    log_append(sprintf("Expense range: %.2f%% to %.2f%%",
                       min(expense_range) * 100, max(expense_range) * 100)) |>
    log_append(sprintf("Grid size: %d x %d = %d combinations",
                       length(growth_range), length(expense_range),
                       length(growth_range) * length(expense_range)))

  start_time <- Sys.time()
  data <- forecast$collect()
  duration <- difftime(Sys.time(), start_time, units = "secs")

  outcomes <- data$outcomes
  audited <- audited |>
    log_append(sprintf("Execution completed in %.3f seconds", duration)) |>
    log_append(sprintf("Outcome range: $%.2f to $%.2f",
                       min(outcomes), max(outcomes))) |>
    log_append(sprintf("Outcome mean: $%.2f", mean(outcomes)))

  logged(data, logged_log(audited))
}

#' Audited Forecast Comparison
#'
#' Compare multiple forecasts with audit logging.
#'
#' @param ... Named LazyForecast objects
#' @param description Comparison description
#' @param parallel Use parallel execution
#' @return Logged comparison result
#'
#' @export
audited_compare_forecasts <- function(..., description = "Forecast comparison",
                                      parallel = TRUE) {
  forecasts <- list(...)

  audited <- logged(NULL) |>
    log_append(description) |>
    log_append(sprintf("Comparing %d scenarios: %s",
                       length(forecasts),
                       paste(names(forecasts), collapse = ", ")))

  start_time <- Sys.time()
  comparison <- compare_forecasts(..., parallel = parallel)
  duration <- difftime(Sys.time(), start_time, units = "secs")

  audited <- audited |>
    log_append(sprintf("Comparison completed in %.3f seconds", duration)) |>
    log_append(sprintf("Best scenario by mean: %s",
                       comparison$scenario[which.max(comparison$mean)])) |>
    log_append(sprintf("Worst scenario by mean: %s",
                       comparison$scenario[which.min(comparison$mean)]))

  logged(comparison, logged_log(audited))
}

#' Bind Logged Forecasts
#'
#' Chain logged forecast operations, accumulating logs.
#'
#' @param logged_lf Logged object containing forecast or result
#' @param fn Function that takes a value and returns a logged object
#' @return New logged object with combined logs
#'
#' @export
bind_logged_forecast <- function(logged_lf, fn) {
  log_bind(logged_lf, fn)
}

#' Map Over Logged Forecast
#'
#' Apply a function to a logged forecast value without modifying the log.
#'
#' @param logged_lf Logged object
#' @param fn Function to apply
#' @return Logged object with transformed value
#'
#' @export
map_logged_forecast <- function(logged_lf, fn) {
  log_map(logged_lf, fn)
}

#' Write Forecast Audit Log
#'
#' Write the forecast audit log to a file.
#'
#' @param logged_lf Logged forecast result
#' @param path File path
#' @param append Append to existing file
#' @return Invisibly returns the logged object
#'
#' @export
write_forecast_audit <- function(logged_lf, path, append = FALSE) {
  write_audit_log(logged_lf, path, append)
}

#' Print Forecast Audit Summary
#'
#' Print a summary of the forecast audit log.
#'
#' @param logged_lf Logged forecast result
#' @return Invisibly returns the logged object
#'
#' @export
print_forecast_audit <- function(logged_lf) {
  if (!inherits(logged_lf, "logged")) {
    rlang::abort("Expected a logged object")
  }

  log <- logged_log(logged_lf)
  cat("=== Forecast Audit Summary ===\n")
  cat("Total log entries:", length(log), "\n\n")

  for (entry in log) {
    cat(entry, "\n")
  }

  invisible(logged_lf)
}
