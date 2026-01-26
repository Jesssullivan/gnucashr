#' Forecasting Expression API
#'
#' User-facing API for building lazy forecasts.
#' All functions return LazyForecast objects for deferred execution.
#'
#' @name forecast-expr
NULL

#' Create Forecast Expression
#'
#' Entry point for building a lazy forecast pipeline.
#' The expression tree is built without loading or computing data.
#'
#' @param source Optional data source (config, tibble, GnuCashDB, BookCollection)
#' @return LazyForecast object
#'
#' @examples
#' \dontrun{
#' # Build expression tree - NO DATA LOADED
#' forecast <- forecast_expr() |>
#'   grow(rate = 0.05, months = 12) |>
#'   monte_carlo(n = 10000)
#'
#' # Inspect without executing
#' forecast$show_plan()
#'
#' # Execute and get results
#' result <- forecast$collect()
#' }
#'
#' @export
forecast_expr <- function(source = NULL) {
  LazyForecast$new(source)
}

#' Create Forecast from GnuCash Book
#'
#' Initialize a lazy forecast from a GnuCash database.
#'
#' @param gc GnuCashDB object
#' @return LazyForecast with GnuCashDB source
#'
#' @examples
#' \dontrun{
#' gc <- read_gnucash("path/to/file.gnucash")
#' forecast <- from_book(gc) |>
#'   grow(rate = 0.05, months = 12)
#' }
#'
#' @export
from_book <- function(gc) {
  if (!inherits(gc, "GnuCashDB")) {
    rlang::abort("gc must be a GnuCashDB object")
  }
  LazyForecast$new(gc)
}

#' Create Forecast from Book Collection
#'
#' Initialize a lazy forecast from a multi-book collection.
#'
#' @param collection BookCollection object
#' @return LazyForecast with BookCollection source
#'
#' @examples
#' \dontrun{
#' collection <- book_collection()
#' collection$add_book("inc", "path/to/inc.gnucash")
#' forecast <- from_collection(collection) |>
#'   grow(rate = 0.05, months = 12)
#' }
#'
#' @export
from_collection <- function(collection) {
  if (!inherits(collection, "BookCollection")) {
    rlang::abort("collection must be a BookCollection object")
  }
  LazyForecast$new(collection)
}

#' Create Forecast from Entity Config
#'
#' Initialize a lazy forecast from entity configuration.
#' Compatible with functions_config.R load_entity_config() output.
#'
#' @param config Entity configuration list
#' @return LazyForecast with config source
#'
#' @examples
#' \dontrun{
#' config <- load_entity_config()
#' forecast <- from_config(config) |>
#'   grow(rate = 0.05, months = 12) |>
#'   monte_carlo(n = 10000)
#' }
#'
#' @export
from_config <- function(config) {
  if (!is.list(config) || !"entities" %in% names(config)) {
    rlang::abort("config must be an entity configuration list")
  }
  LazyForecast$new(config)
}

#' Create Forecast from Data Frame
#'
#' Initialize a lazy forecast from a tibble or data frame.
#'
#' @param data Data frame with financial data
#' @return LazyForecast with data frame source
#'
#' @export
from_data <- function(data) {
  if (!is.data.frame(data)) {
    rlang::abort("data must be a data frame")
  }
  LazyForecast$new(tibble::as_tibble(data))
}

# Pipe-Friendly Operations ----

#' Grow Revenue (Pipe-Friendly)
#'
#' Add growth projection to a lazy forecast.
#'
#' @param lf LazyForecast object
#' @param rate Monthly growth rate (e.g., 0.05 for 5%)
#' @param months Number of months to project
#' @param compound Use compound growth (default TRUE)
#' @return LazyForecast (for chaining)
#'
#' @export
lf_grow <- function(lf, rate, months, compound = TRUE) {
  if (!inherits(lf, "LazyForecast")) {
    rlang::abort("lf must be a LazyForecast object")
  }
  lf$grow(rate, months, compound)
  lf
}

#' Filter Forecast Data (Pipe-Friendly)
#'
#' Add filter operation to a lazy forecast.
#'
#' @param lf LazyForecast object
#' @param ... Filter conditions
#' @return LazyForecast (for chaining)
#'
#' @export
lf_filter <- function(lf, ...) {
  if (!inherits(lf, "LazyForecast")) {
    rlang::abort("lf must be a LazyForecast object")
  }
  lf$filter(...)
  lf
}

#' Monte Carlo Simulation (Pipe-Friendly)
#'
#' Add Monte Carlo simulation to a lazy forecast.
#'
#' @param lf LazyForecast object
#' @param n Number of simulations
#' @param growth_mean Mean monthly growth rate
#' @param growth_sd Growth rate standard deviation
#' @param expense_mean Mean expense ratio
#' @param expense_sd Expense ratio standard deviation
#' @param months Number of months to simulate
#' @param seed Random seed for reproducibility
#' @return LazyForecast (for chaining)
#'
#' @examples
#' \dontrun{
#' forecast <- forecast_expr() |>
#'   lf_monte_carlo(
#'     n = 10000,
#'     growth_mean = 0.05,
#'     growth_sd = 0.03,
#'     seed = 42
#'   )
#' result <- forecast$collect()
#' result$summary$p50  # Median outcome
#' }
#'
#' @export
lf_monte_carlo <- function(lf, n = 10000,
                           growth_mean = 0.05, growth_sd = 0.03,
                           expense_mean = 0.7, expense_sd = 0.05,
                           months = 12, seed = 42L) {
  if (!inherits(lf, "LazyForecast")) {
    rlang::abort("lf must be a LazyForecast object")
  }

  params <- list(
    growth = list(mean = growth_mean, sd = growth_sd),
    expense = list(mean = expense_mean, sd = expense_sd),
    months = months
  )

  lf$monte_carlo(n = n, params = params, seed = seed)
  lf
}

#' Sensitivity Analysis (Pipe-Friendly)
#'
#' Add sensitivity grid analysis to a lazy forecast.
#'
#' @param lf LazyForecast object
#' @param growth_range Vector of growth rates to test
#' @param expense_range Vector of expense ratios to test
#' @param months Number of months to project
#' @return LazyForecast (for chaining)
#'
#' @examples
#' \dontrun{
#' forecast <- forecast_expr() |>
#'   lf_sensitivity(
#'     growth_range = seq(-0.02, 0.08, by = 0.01),
#'     expense_range = seq(0.5, 0.9, by = 0.05)
#'   )
#' result <- forecast$collect()
#' # result$outcomes is a matrix of results
#' }
#'
#' @export
lf_sensitivity <- function(lf,
                           growth_range = seq(-0.02, 0.08, by = 0.01),
                           expense_range = seq(0.5, 0.9, by = 0.05),
                           months = 12) {
  if (!inherits(lf, "LazyForecast")) {
    rlang::abort("lf must be a LazyForecast object")
  }

  param_ranges <- list(
    growth = growth_range,
    expense = expense_range,
    months = months
  )

  lf$sensitivity(param_ranges)
  lf
}

#' Create Named Scenario (Pipe-Friendly)
#'
#' Add a named scenario to a lazy forecast.
#'
#' @param lf LazyForecast object
#' @param name Scenario name
#' @param growth_boost Growth rate adjustment
#' @param expense_change Expense rate adjustment
#' @param ... Additional scenario parameters
#' @return LazyForecast (for chaining)
#'
#' @export
lf_scenario <- function(lf, name, growth_boost = 0, expense_change = 0, ...) {
  if (!inherits(lf, "LazyForecast")) {
    rlang::abort("lf must be a LazyForecast object")
  }

  params <- list(
    growth_boost = growth_boost,
    expense_change = expense_change,
    ...
  )

  lf$scenario(name, params)
  lf
}

#' Collect Forecast Results
#'
#' Execute the lazy forecast and return materialized results.
#'
#' @param lf LazyForecast object
#' @param parallel Use parallel execution (default TRUE)
#' @return Materialized forecast results
#'
#' @export
lf_collect <- function(lf, parallel = TRUE) {
  if (!inherits(lf, "LazyForecast")) {
    rlang::abort("lf must be a LazyForecast object")
  }
  lf$collect(parallel = parallel)
}

# Comparison Functions ----

#' Compare Multiple Forecasts
#'
#' Run and compare multiple lazy forecasts.
#'
#' @param ... Named LazyForecast objects to compare
#' @param parallel Use parallel execution
#' @return Comparison tibble
#'
#' @examples
#' \dontrun{
#' base <- forecast_expr(config) |> lf_monte_carlo(n = 1000)
#' optimistic <- forecast_expr(config) |>
#'   lf_monte_carlo(n = 1000, growth_mean = 0.08)
#'
#' compare_forecasts(
#'   base = base,
#'   optimistic = optimistic
#' )
#' }
#'
#' @export
compare_forecasts <- function(..., parallel = TRUE) {
  forecasts <- list(...)

  # Validate all are LazyForecast
  for (name in names(forecasts)) {
    if (!inherits(forecasts[[name]], "LazyForecast")) {
      rlang::abort(sprintf("'%s' is not a LazyForecast object", name))
    }
  }

  # Collect all in parallel if possible
  results <- purrr::imap(forecasts, function(lf, name) {
    result <- lf$collect(parallel = parallel)

    # Extract key metrics based on result type
    if (is.list(result) && "summary" %in% names(result)) {
      # Monte Carlo result
      tibble::tibble(
        scenario = name,
        mean = result$summary$mean %||% NA_real_,
        p5 = result$summary$p5 %||% NA_real_,
        p25 = result$summary$p25 %||% NA_real_,
        p50 = result$summary$p50 %||% NA_real_,
        p75 = result$summary$p75 %||% NA_real_,
        p95 = result$summary$p95 %||% NA_real_
      )
    } else if (is.list(result) && "outcomes" %in% names(result)) {
      # Sensitivity result
      outcomes <- result$outcomes
      tibble::tibble(
        scenario = name,
        mean = mean(outcomes, na.rm = TRUE),
        min = min(outcomes, na.rm = TRUE),
        max = max(outcomes, na.rm = TRUE),
        range = max(outcomes, na.rm = TRUE) - min(outcomes, na.rm = TRUE)
      )
    } else if (is.data.frame(result)) {
      # Projection result
      tibble::tibble(
        scenario = name,
        mean = mean(result$cumulative_revenue %||% result$projected_revenue, na.rm = TRUE),
        total = sum(result$projected_revenue, na.rm = TRUE)
      )
    } else {
      tibble::tibble(scenario = name)
    }
  })

  dplyr::bind_rows(results)
}

#' Quick Monte Carlo Forecast
#'
#' Convenience function for common Monte Carlo pattern.
#'
#' @param source Data source (config, tibble, or path)
#' @param n Number of simulations
#' @param months Number of months
#' @param growth_mean Mean growth rate
#' @param growth_sd Growth rate standard deviation
#' @param seed Random seed
#' @return Monte Carlo results
#'
#' @examples
#' \dontrun{
#' # Quick forecast from config
#' result <- quick_monte_carlo(config, n = 10000, months = 12)
#' result$summary$p50  # Median cumulative cash
#' }
#'
#' @export
quick_monte_carlo <- function(source,
                              n = 10000,
                              months = 12,
                              growth_mean = 0.05,
                              growth_sd = 0.03,
                              seed = 42L) {
  forecast_expr(source) |>
    lf_monte_carlo(
      n = n,
      growth_mean = growth_mean,
      growth_sd = growth_sd,
      months = months,
      seed = seed
    ) |>
    lf_collect()
}

#' Quick Sensitivity Analysis
#'
#' Convenience function for common sensitivity analysis pattern.
#'
#' @param source Data source
#' @param growth_range Growth rate range
#' @param expense_range Expense ratio range
#' @param months Number of months
#' @return Sensitivity results
#'
#' @export
quick_sensitivity <- function(source,
                              growth_range = seq(-0.02, 0.08, by = 0.01),
                              expense_range = seq(0.5, 0.9, by = 0.05),
                              months = 12) {
  forecast_expr(source) |>
    lf_sensitivity(
      growth_range = growth_range,
      expense_range = expense_range,
      months = months
    ) |>
    lf_collect()
}
