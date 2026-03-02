# test-monte-carlo.R - Tests for Monte Carlo simulation functions
#
# Note: These tests require RcppParallel to be installed and working.
# Skip if the C++ code is not compiled.

test_that("monte_carlo_parallel returns expected structure", {
  skip_if_not(exists("monte_carlo_parallel", mode = "function"),
              message = "Rcpp exports not available")

  result <- monte_carlo_parallel(
    base_revenue = 100000,
    base_expense_rate = 0.7,
    n_sims = 100,
    n_periods = 12,
    growth_mean = 0.05,
    growth_sd = 0.02,
    seed = 42
  )

  expect_type(result, "list")
  expect_true("results" %in% names(result))
  expect_true("final_cash" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("params" %in% names(result))

  # Check dimensions
  expect_equal(nrow(result$results), 100)
  expect_equal(ncol(result$results), 12)
  expect_equal(length(result$final_cash), 100)
})

test_that("monte_carlo_parallel is reproducible with same seed", {
  skip_if_not(exists("monte_carlo_parallel", mode = "function"),
              message = "Rcpp exports not available")

  result1 <- monte_carlo_parallel(
    base_revenue = 100000,
    base_expense_rate = 0.7,
    n_sims = 50,
    n_periods = 6,
    seed = 123
  )

  result2 <- monte_carlo_parallel(
    base_revenue = 100000,
    base_expense_rate = 0.7,
    n_sims = 50,
    n_periods = 6,
    seed = 123
  )

  # Results should be identical with same seed
  expect_equal(result1$final_cash, result2$final_cash)
  expect_equal(result1$summary$mean, result2$summary$mean)
})

test_that("monte_carlo_parallel produces different results with different seeds", {
  skip_if_not(exists("monte_carlo_parallel", mode = "function"),
              message = "Rcpp exports not available")

  result1 <- monte_carlo_parallel(
    base_revenue = 100000,
    base_expense_rate = 0.7,
    n_sims = 50,
    seed = 42
  )

  result2 <- monte_carlo_parallel(
    base_revenue = 100000,
    base_expense_rate = 0.7,
    n_sims = 50,
    seed = 123
  )

  # Results should differ
  expect_false(all(result1$final_cash == result2$final_cash))
})

test_that("monte_carlo_parallel summary statistics are reasonable", {
  skip_if_not(exists("monte_carlo_parallel", mode = "function"),
              message = "Rcpp exports not available")

  result <- monte_carlo_parallel(
    base_revenue = 100000,
    base_expense_rate = 0.7,
    n_sims = 1000,
    n_periods = 12,
    growth_mean = 0.05,
    growth_sd = 0.01,
    seed = 42
  )

  summary <- result$summary

  # Quantiles should be ordered
  expect_true(summary$p5 <= summary$p25)
  expect_true(summary$p25 <= summary$p50)
  expect_true(summary$p50 <= summary$p75)
  expect_true(summary$p75 <= summary$p95)

  # Mean should be close to median for normal-ish distribution
  expect_true(abs(summary$mean - summary$p50) / summary$mean < 0.2)

  # Min and max should bound the quantiles
  expect_true(summary$min <= summary$p5)
  expect_true(summary$max >= summary$p95)
})

test_that("monte_carlo_multi_entity returns expected structure", {
  skip_if_not(exists("monte_carlo_multi_entity", mode = "function"),
              message = "Rcpp exports not available")

  result <- monte_carlo_multi_entity(
    base_revenues = c(50000, 70000, 7000, 5000),
    growth_means = c(0.05, 0.08, 0.05, 0.02),
    growth_sds = c(0.03, 0.04, 0.02, 0.01),
    expense_rates = c(0.65, 0.70, 0.30, 0.80),
    n_sims = 100,
    n_periods = 12,
    seed = 42
  )

  expect_type(result, "list")
  expect_true("entity_results" %in% names(result))
  expect_true("total_cash" %in% names(result))
  expect_true("summary" %in% names(result))

  # Check dimensions: n_sims x (n_entities * n_periods)
  expect_equal(nrow(result$entity_results), 100)
  expect_equal(ncol(result$entity_results), 4 * 12)  # 4 entities * 12 periods
  expect_equal(length(result$total_cash), 100)
})

test_that("monte_carlo_multi_entity validates input lengths", {
  skip_if_not(exists("monte_carlo_multi_entity", mode = "function"),
              message = "Rcpp exports not available")

  # Mismatched lengths should error
  expect_error(
    monte_carlo_multi_entity(
      base_revenues = c(50000, 70000),
      growth_means = c(0.05),  # Wrong length
      growth_sds = c(0.03, 0.04),
      expense_rates = c(0.65, 0.70)
    ),
    "same length"
  )
})

test_that("quick_monte_carlo convenience function works", {
  skip_if_not(exists("monte_carlo_parallel", mode = "function"),
              message = "Rcpp exports not available")

  # Create a simple config
  config <- list(
    entities = list(
      products = list(revenue = 50000, operating_rate = 0.65),
      services = list(revenue = 70000, operating_rate = 0.70)
    ),
    growth = list(products = 0.05, services = 0.08)
  )

  result <- quick_monte_carlo(config, n = 100, months = 6, seed = 42)

  expect_type(result, "list")
  expect_true("summary" %in% names(result))
})

test_that("LazyForecast collect() executes Monte Carlo", {
  skip_if_not(exists("monte_carlo_parallel", mode = "function"),
              message = "Rcpp exports not available")

  config <- list(
    entities = list(
      products = list(revenue = 50000, operating_rate = 0.65)
    ),
    growth = list(products = 0.05)
  )

  lf <- forecast_expr(config) |>
    lf_monte_carlo(n = 100, months = 6, seed = 42)

  result <- lf$collect()

  expect_type(result, "list")
  expect_true("summary" %in% names(result))
  expect_true(lf$metadata()$materialized)
})
