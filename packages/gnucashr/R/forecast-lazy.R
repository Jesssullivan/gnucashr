#' LazyForecast R6 Class
#'
#' Monadic lazy evaluation for financial forecasting.
#' Builds an expression tree (AST) without executing until collect() is called.
#' Integrates with Result and Logger monads for safe, auditable computation.
#'
#' @name LazyForecast
#' @export
LazyForecast <- R6::R6Class("LazyForecast",
  public = list(
    #' @description Create a new LazyForecast
    #' @param source Data source (config, tibble, GnuCashDB, BookCollection, or NULL)
    initialize = function(source = NULL) {
      private$.source <- source
      private$.ast <- list()
      private$.cache <- NULL
      private$.metadata <- list(
        created = Sys.time(),
        materialized = FALSE
      )
      invisible(self)
    },

    # Monadic Operations ----

    #' @description Monadic bind - chain computation
    #' @param fn Function that takes a value and returns a LazyForecast
    #' @return New LazyForecast with combined AST
    bind = function(fn) {
      new_lf <- LazyForecast$new(private$.source)
      new_lf$.__enclos_env__$private$.ast <- c(
        private$.ast,
        list(list(op = "bind", fn = fn))
      )
      new_lf
    },

    #' @description Map over value (functor)
    #' @param fn Function to apply to materialized value
    #' @return New LazyForecast with transform operation added
    map = function(fn) {
      new_lf <- LazyForecast$new(private$.source)
      new_lf$.__enclos_env__$private$.ast <- c(
        private$.ast,
        list(list(op = "map", fn = fn))
      )
      new_lf
    },

    # Forecasting Operations (build AST, don't execute) ----

    #' @description Add growth projection to AST
    #' @param rate Monthly growth rate
    #' @param months Number of months to project
    #' @param compound Whether to compound (default TRUE)
    #' @return Self (for chaining)
    grow = function(rate, months, compound = TRUE) {
      private$.ast <- c(private$.ast, list(list(
        op = "grow",
        rate = rate,
        months = months,
        compound = compound
      )))
      private$.cache <- NULL
      invisible(self)
    },

    #' @description Add filter operation to AST
    #' @param predicate Filter predicate expression or function
    #' @return Self (for chaining)
    filter = function(predicate) {
      private$.ast <- c(private$.ast, list(list(
        op = "filter",
        predicate = rlang::enquo(predicate)
      )))
      private$.cache <- NULL
      invisible(self)
    },

    #' @description Add aggregation operation to AST
    #' @param ... Column specifications
    #' @param .fns Aggregation functions
    #' @return Self (for chaining)
    aggregate = function(..., .fns = list(sum = sum, mean = mean)) {
      private$.ast <- c(private$.ast, list(list(
        op = "aggregate",
        cols = rlang::enquos(...),
        fns = .fns
      )))
      private$.cache <- NULL
      invisible(self)
    },

    #' @description Add named scenario to AST
    #' @param name Scenario name
    #' @param params Scenario parameters
    #' @return Self (for chaining)
    scenario = function(name, params) {
      private$.ast <- c(private$.ast, list(list(
        op = "scenario",
        name = name,
        params = params
      )))
      private$.cache <- NULL
      invisible(self)
    },

    # Parallel Operations (flag for Rcpp execution) ----

    #' @description Add Monte Carlo simulation to AST
    #' @param n Number of simulations
    #' @param params List with growth, expense parameters
    #' @param seed Random seed for reproducibility
    #' @return Self (for chaining)
    monte_carlo = function(n = 10000, params = list(), seed = 42L) {
      private$.ast <- c(private$.ast, list(list(
        op = "monte_carlo",
        n = n,
        params = params,
        seed = seed,
        parallel = TRUE
      )))
      private$.cache <- NULL
      invisible(self)
    },

    #' @description Add sensitivity analysis to AST
    #' @param param_ranges Named list of parameter ranges
    #' @return Self (for chaining)
    sensitivity = function(param_ranges) {
      private$.ast <- c(private$.ast, list(list(
        op = "sensitivity",
        param_ranges = param_ranges,
        parallel = TRUE
      )))
      private$.cache <- NULL
      invisible(self)
    },

    # Materialization ----

    #' @description Execute the AST and return materialized result

#' @param parallel Use parallel execution (default TRUE)
#' @param n_threads Number of threads (NULL for auto)
#' @return Materialized result
    collect = function(parallel = TRUE, n_threads = NULL) {
      if (!is.null(private$.cache)) {
        return(private$.cache)
      }

      result <- private$.execute_ast(parallel, n_threads)
      private$.cache <- result
      private$.metadata$materialized <- TRUE
      private$.metadata$collected_at <- Sys.time()
      result
    },

    #' @description Alias for collect()
    force = function() {
      self$collect()
    },

    # Inspection ----

    #' @description Print the AST without executing
    #' @return Self (invisibly)
    show_plan = function() {
      cat("<LazyForecast Plan>\n")

      # Source info
      source_type <- if (is.null(private$.source)) {
        "NULL"
      } else if (inherits(private$.source, "GnuCashDB")) {
        "GnuCashDB"
      } else if (inherits(private$.source, "BookCollection")) {
        "BookCollection"
      } else if (is.data.frame(private$.source)) {
        sprintf("tibble (%d rows)", nrow(private$.source))
      } else if (is.list(private$.source)) {
        "list (entity config)"
      } else {
        class(private$.source)[1]
      }
      cat("Source:", source_type, "\n")

      # Operations
      if (length(private$.ast) == 0) {
        cat("Operations: (none)\n")
      } else {
        cat("Operations:\n")
        for (i in seq_along(private$.ast)) {
          op <- private$.ast[[i]]
          desc <- private$.format_operation(op)
          cat(sprintf("  %d. %s\n", i, desc))
        }
      }

      # Status
      cat("Materialized:", private$.metadata$materialized, "\n")
      if (private$.metadata$materialized) {
        cat("Collected at:", format(private$.metadata$collected_at), "\n")
      }

      invisible(self)
    },

    #' @description Get metadata about the forecast
    #' @return List of metadata
    metadata = function() {
      list(
        source_type = class(private$.source)[1],
        n_operations = length(private$.ast),
        operations = purrr::map_chr(private$.ast, ~ .x$op),
        created = private$.metadata$created,
        materialized = private$.metadata$materialized,
        collected_at = private$.metadata$collected_at
      )
    },

    #' @description Get the AST
    #' @return List of AST nodes
    get_ast = function() {
      private$.ast
    },

    #' @description Get the source
    #' @return Source data
    get_source = function() {
      private$.source
    },

    #' @description Print method
    print = function() {
      self$show_plan()
    }
  ),

  private = list(
    .source = NULL,
    .ast = list(),
    .cache = NULL,
    .metadata = list(),

    # Execute the accumulated AST
    .execute_ast = function(parallel, n_threads) {
      # Start with source data
      data <- private$.extract_source_data()

      # Execute each operation in sequence
      for (op in private$.ast) {
        data <- private$.execute_operation(op, data, parallel, n_threads)
      }

      data
    },

    # Extract base data from source
    .extract_source_data = function() {
      source <- private$.source

      if (is.null(source)) {
        return(NULL)
      }

      if (inherits(source, "GnuCashDB")) {
        # Extract trial balance as base
        return(trial_balance(source))
      }

      if (inherits(source, "BookCollection")) {
        # Extract consolidated trial balance
        return(source$consolidated_trial_balance())
      }

      if (is.data.frame(source)) {
        return(source)
      }

      if (is.list(source) && "entities" %in% names(source)) {
        # Entity config - convert to tibble
        return(private$.config_to_tibble(source))
      }

      source
    },

    # Convert entity config to tibble
    .config_to_tibble = function(config) {
      entities <- config$entities
      growth <- config$growth

      purrr::imap_dfr(entities, function(ent, name) {
        tibble::tibble(
          entity = name,
          revenue = ent$revenue %||% 0,
          operating_rate = ent$operating_rate %||% 0.7,
          cogs_rate = ent$cogs_rate %||% 0,
          growth_rate = growth[[name]] %||% 0.05
        )
      })
    },

    # Execute a single operation
    .execute_operation = function(op, data, parallel, n_threads) {
      switch(op$op,
        "grow" = private$.execute_grow(op, data),
        "filter" = private$.execute_filter(op, data),
        "aggregate" = private$.execute_aggregate(op, data),
        "scenario" = private$.execute_scenario(op, data),
        "monte_carlo" = private$.execute_monte_carlo(op, data, parallel),
        "sensitivity" = private$.execute_sensitivity(op, data, parallel),
        "map" = op$fn(data),
        "bind" = {
          result <- op$fn(data)
          if (inherits(result, "LazyForecast")) {
            result$collect(parallel, n_threads)
          } else {
            result
          }
        },
        data  # Unknown op, pass through
      )
    },

    # Execute growth projection
    .execute_grow = function(op, data) {
      if (is.null(data)) {
        # No source data - return projection params
        return(list(
          type = "growth_projection",
          rate = op$rate,
          months = op$months,
          compound = op$compound
        ))
      }

      if (is.data.frame(data) && "revenue" %in% names(data)) {
        # Project revenue forward
        months <- op$months
        rate <- op$rate

        projections <- purrr::map_dfr(seq_len(months), function(m) {
          factor <- if (op$compound) (1 + rate)^m else (1 + rate * m)
          data |>
            dplyr::mutate(
              month = m,
              projected_revenue = revenue * factor,
              cumulative_revenue = projected_revenue * m
            )
        })

        return(projections)
      }

      data
    },

    # Execute filter
    .execute_filter = function(op, data) {
      if (!is.data.frame(data)) {
        return(data)
      }
      dplyr::filter(data, !!op$predicate)
    },

    # Execute aggregation
    .execute_aggregate = function(op, data) {
      if (!is.data.frame(data)) {
        return(data)
      }
      # Simple aggregation - can be extended
      dplyr::summarize(data, dplyr::across(
        dplyr::where(is.numeric),
        op$fns
      ))
    },

    # Execute scenario
    .execute_scenario = function(op, data) {
      list(
        name = op$name,
        params = op$params,
        base_data = data
      )
    },

    # Execute Monte Carlo using Rcpp backend
    .execute_monte_carlo = function(op, data, parallel) {
      params <- op$params

      # Extract base values from data
      if (is.data.frame(data) && "revenue" %in% names(data)) {
        base_revenue <- sum(data$revenue, na.rm = TRUE)
        base_expense_rate <- mean(data$operating_rate, na.rm = TRUE)
      } else if (is.list(data) && "entities" %in% names(data)) {
        # Entity config
        base_revenue <- sum(purrr::map_dbl(data$entities, ~ .x$revenue %||% 0))
        base_expense_rate <- mean(purrr::map_dbl(data$entities, ~ .x$operating_rate %||% 0.7))
      } else {
        base_revenue <- params$base_revenue %||% 100000
        base_expense_rate <- params$base_expense_rate %||% 0.7
      }

      # Call Rcpp parallel function
      monte_carlo_parallel(
        base_revenue = base_revenue,
        base_expense_rate = base_expense_rate,
        n_sims = op$n,
        n_periods = params$months %||% 12,
        growth_mean = params$growth$mean %||% 0.05,
        growth_sd = params$growth$sd %||% 0.03,
        expense_mean = params$expense$mean %||% base_expense_rate,
        expense_sd = params$expense$sd %||% 0.05,
        seed = op$seed
      )
    },

    # Execute sensitivity analysis using Rcpp backend
    .execute_sensitivity = function(op, data, parallel) {
      ranges <- op$param_ranges

      # Extract base values
      if (is.data.frame(data) && "revenue" %in% names(data)) {
        base_revenue <- sum(data$revenue, na.rm = TRUE)
        base_expense_rate <- mean(data$operating_rate, na.rm = TRUE)
      } else {
        base_revenue <- 100000
        base_expense_rate <- 0.7
      }

      growth_range <- ranges$growth %||% seq(-0.02, 0.08, by = 0.01)
      expense_range <- ranges$expense %||% seq(0.5, 0.9, by = 0.05)

      # Call Rcpp parallel function
      parallel_sensitivity_grid(
        base_revenue = base_revenue,
        base_expense_rate = base_expense_rate,
        growth_range = growth_range,
        expense_range = expense_range,
        n_periods = ranges$months %||% 12
      )
    },

    # Format operation for display
    .format_operation = function(op) {
      switch(op$op,
        "grow" = sprintf("grow (rate=%.2f%%, months=%d, compound=%s)",
                         op$rate * 100, op$months, op$compound),
        "filter" = "filter (predicate)",
        "aggregate" = sprintf("aggregate (%d functions)", length(op$fns)),
        "scenario" = sprintf("scenario '%s'", op$name),
        "monte_carlo" = sprintf("monte_carlo (n=%d, seed=%d)", op$n, op$seed),
        "sensitivity" = sprintf("sensitivity (%d parameters)",
                                length(op$param_ranges)),
        "map" = "map (function)",
        "bind" = "bind (function)",
        op$op
      )
    }
  )
)

#' Create a new LazyForecast
#'
#' Factory function for creating LazyForecast objects.
#'
#' @param source Data source (config, tibble, GnuCashDB, BookCollection)
#' @return LazyForecast object
#' @export
lazy_forecast <- function(source = NULL) {
  LazyForecast$new(source)
}

#' @export
print.LazyForecast <- function(x, ...) {
  x$show_plan()
  invisible(x)
}
