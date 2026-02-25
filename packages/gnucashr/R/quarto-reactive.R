#' Quarto Reactive Data Sources
#'
#' Functions for creating reactive data sources in Quarto dashboards.
#' These wrap gnucashr data access with Shiny-compatible reactivity.
#'
#' @name quarto-reactive
#' @keywords internal
NULL

#' Create Reactive GnuCash Connection
#'
#' Creates a reactive GnuCash database connection that can be used
#' in Shiny or Quarto interactive documents.
#'
#' @param path Path to GnuCash file (reactive input)
#' @param refresh_interval Refresh interval in milliseconds (default: 60000)
#' @return A reactive expression returning a GnuCashDB object
#' @export
#' @examples
#' \dontrun{
#' # In a Quarto dashboard
#' gc <- reactive_gnucash(input$gnucash_file)
#' }
reactive_gnucash <- function(path, refresh_interval = 60000) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    rlang::abort("Package 'shiny' is required for reactive data sources")
  }

  shiny::reactive({
    shiny::invalidateLater(refresh_interval)
    req_path <- shiny::req(path)
    read_gnucash(req_path)
  })
}

#' Create Reactive Trial Balance
#'
#' Creates a reactive trial balance that updates when the underlying
#' GnuCash data changes.
#'
#' @param gc_reactive Reactive GnuCashDB object
#' @param as_of_reactive Reactive date (optional)
#' @return A reactive expression returning a trial balance tibble
#' @export
#' @examples
#' \dontrun{
#' gc <- reactive_gnucash(input$gnucash_file)
#' tb <- reactive_trial_balance(gc, reactive(input$as_of_date))
#' }
reactive_trial_balance <- function(gc_reactive, as_of_reactive = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    rlang::abort("Package 'shiny' is required for reactive data sources")
  }

  shiny::reactive({
    gc <- gc_reactive()
    as_of <- if (!is.null(as_of_reactive)) as_of_reactive() else Sys.Date()
    trial_balance(gc, as_of = as_of)
  })
}

#' Create Reactive Balance Sheet
#'
#' Creates a reactive balance sheet that updates when the underlying
#' GnuCash data changes. First generates a trial balance, then creates
#' the balance sheet from it.
#'
#' @param gc_reactive Reactive GnuCashDB object
#' @param as_of_reactive Reactive date (optional)
#' @return A reactive expression returning a balance sheet list
#' @export
reactive_balance_sheet <- function(gc_reactive, as_of_reactive = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    rlang::abort("Package 'shiny' is required for reactive data sources")
  }

  shiny::reactive({
    gc <- gc_reactive()
    as_of <- if (!is.null(as_of_reactive)) as_of_reactive() else Sys.Date()
    tb <- trial_balance(gc, as_of = as_of)
    balance_sheet(tb)
  })
}

#' Create Reactive Income Statement
#'
#' Creates a reactive income statement for a date range.
#' First generates activity data, then creates the income statement from it.
#'
#' @param gc_reactive Reactive GnuCashDB object
#' @param start_reactive Reactive start date
#' @param end_reactive Reactive end date
#' @return A reactive expression returning an income statement list
#' @export
reactive_income_statement <- function(gc_reactive, start_reactive, end_reactive) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    rlang::abort("Package 'shiny' is required for reactive data sources")
  }

  shiny::reactive({
    gc <- gc_reactive()
    start <- start_reactive()
    end <- end_reactive()
    activity <- income_statement_activity(gc, start_date = start, end_date = end)
    income_statement(activity)
  })
}

#' Create Reactive Account Transactions
#'
#' Creates a reactive list of transactions for a specific account.
#'
#' @param gc_reactive Reactive GnuCashDB object
#' @param account_reactive Reactive account identifier (GUID or path)
#' @param start_reactive Optional reactive start date
#' @param end_reactive Optional reactive end date
#' @return A reactive expression returning transactions tibble
#' @export
reactive_account_transactions <- function(gc_reactive,
                                          account_reactive,
                                          start_reactive = NULL,
                                          end_reactive = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    rlang::abort("Package 'shiny' is required for reactive data sources")
  }

  shiny::reactive({
    gc <- gc_reactive()
    account <- account_reactive()
    start <- if (!is.null(start_reactive)) start_reactive() else NULL
    end <- if (!is.null(end_reactive)) end_reactive() else NULL
    account_transactions(gc, account, start = start, end = end)
  })
}

#' Create Reactive Book Collection
#'
#' Creates a reactive book collection for multi-entity dashboards.
#'
#' @param paths_reactive Reactive list of GnuCash file paths with names
#' @param entity_config Optional entity configuration
#' @return A reactive expression returning a BookCollection object
#' @export
reactive_book_collection <- function(paths_reactive, entity_config = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    rlang::abort("Package 'shiny' is required for reactive data sources")
  }

  shiny::reactive({
    paths <- paths_reactive()
    collection <- book_collection(entity_config)
    for (name in names(paths)) {
      collection$add_book(name, paths[[name]])
    }
    collection
  })
}

#' Create Reactive Consolidated Trial Balance
#'
#' Creates a reactive consolidated trial balance across multiple books.
#'
#' @param collection_reactive Reactive BookCollection object
#' @param as_of_reactive Reactive date
#' @param eliminate_ic Whether to eliminate intercompany transactions
#' @return A reactive expression returning consolidated trial balance
#' @export
reactive_consolidated_tb <- function(collection_reactive,
                                     as_of_reactive = NULL,
                                     eliminate_ic = TRUE) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    rlang::abort("Package 'shiny' is required for reactive data sources")
  }

  shiny::reactive({
    collection <- collection_reactive()
    as_of <- if (!is.null(as_of_reactive)) as_of_reactive() else Sys.Date()
    collection$consolidated_trial_balance(as_of = as_of, eliminate_ic = eliminate_ic)
  })
}

#' Create Reactive Forecast
#'
#' Creates a reactive forecast that recalculates when parameters change.
#'
#' @param config_reactive Reactive entity configuration
#' @param growth_reactive Reactive growth rate
#' @param months Number of months to forecast
#' @return A reactive expression returning forecast data
#' @export
reactive_forecast <- function(config_reactive, growth_reactive, months = 12) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    rlang::abort("Package 'shiny' is required for reactive data sources")
  }

  shiny::reactive({
    config <- config_reactive()
    growth <- growth_reactive()

    forecast_expr(config) |>
      lf_grow(rate = growth, months = months) |>
      lf_collect()
  })
}

#' Create Reactive Monte Carlo Simulation
#'
#' Creates a reactive Monte Carlo simulation with configurable parameters.
#'
#' @param config_reactive Reactive entity configuration
#' @param n_sims Number of simulations
#' @param growth_mean_reactive Reactive mean growth rate
#' @param growth_sd_reactive Reactive growth rate standard deviation
#' @param seed Random seed for reproducibility
#' @return A reactive expression returning simulation results
#' @export
reactive_monte_carlo <- function(config_reactive,
                                  n_sims = 10000,
                                  growth_mean_reactive,
                                  growth_sd_reactive,
                                  seed = 42) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    rlang::abort("Package 'shiny' is required for reactive data sources")
  }

  shiny::reactive({
    config <- config_reactive()
    growth_mean <- growth_mean_reactive()
    growth_sd <- growth_sd_reactive()

    forecast_expr(config) |>
      lf_monte_carlo(
        n = n_sims,
        growth_mean = growth_mean,
        growth_sd = growth_sd,
        seed = seed
      ) |>
      lf_collect()
  })
}
