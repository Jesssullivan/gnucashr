# test-parallel.R - Tests for parallel scenario functions
#
# Note: These tests require RcppParallel to be installed and working.
# Skip if the C++ code is not compiled.

test_that("parallel_project_scenarios returns expected structure", {
  skip_if_not(exists("parallel_project_scenarios", mode = "function"),
              message = "Rcpp exports not available")

  base_values <- c(50000, 70000, 7000, 5000)
  growth_matrix <- matrix(
    c(0.03, 0.05, 0.08,    # products scenarios
      0.05, 0.08, 0.10,    # services scenarios
      0.02, 0.05, 0.07,    # software scenarios
      0.01, 0.02, 0.03),   # operations scenarios
    nrow = 3, ncol = 4, byrow = FALSE
  )

  result <- parallel_project_scenarios(
    base_values = base_values,
    growth_matrix = growth_matrix,
    n_periods = 12
  )

  expect_type(result, "list")
  expect_true("results" %in% names(result))
  expect_true("final_totals" %in% names(result))

  # Check dimensions
  expect_equal(nrow(result$results), 3)  # 3 scenarios
  expect_equal(ncol(result$results), 4 * 12)  # 4 entities * 12 periods
  expect_equal(length(result$final_totals), 3)
})

test_that("parallel_project_scenarios validates dimensions", {
  skip_if_not(exists("parallel_project_scenarios", mode = "function"),
              message = "Rcpp exports not available")

  base_values <- c(50000, 70000)

  # Wrong number of columns in growth_matrix
  bad_matrix <- matrix(c(0.03, 0.05), nrow = 1, ncol = 2)
  expect_silent(  # Should work - dimensions match
    parallel_project_scenarios(base_values, bad_matrix, n_periods = 6)
  )

  # Mismatch
  bad_matrix2 <- matrix(c(0.03, 0.05, 0.07), nrow = 1, ncol = 3)
  expect_error(
    parallel_project_scenarios(base_values, bad_matrix2, n_periods = 6),
    "columns"
  )
})

test_that("parallel_sensitivity_grid returns expected structure", {
  skip_if_not(exists("parallel_sensitivity_grid", mode = "function"),
              message = "Rcpp exports not available")

  growth_range <- seq(-0.02, 0.08, by = 0.02)  # 6 values
  expense_range <- seq(0.5, 0.9, by = 0.1)      # 5 values

  result <- parallel_sensitivity_grid(
    base_revenue = 100000,
    base_expense_rate = 0.7,
    growth_range = growth_range,
    expense_range = expense_range,
    n_periods = 12
  )

  expect_type(result, "list")
  expect_true("outcomes" %in% names(result))
  expect_true("growth_range" %in% names(result))
  expect_true("expense_range" %in% names(result))

  # Check dimensions
  expect_equal(nrow(result$outcomes), 6)
  expect_equal(ncol(result$outcomes), 5)
})

test_that("parallel_sensitivity_grid outcome values are reasonable", {
  skip_if_not(exists("parallel_sensitivity_grid", mode = "function"),
              message = "Rcpp exports not available")

  result <- parallel_sensitivity_grid(
    base_revenue = 100000,
    base_expense_rate = 0.7,
    growth_range = c(0.00, 0.05, 0.10),
    expense_range = c(0.5, 0.7, 0.9),
    n_periods = 12
  )

  outcomes <- result$outcomes

  # Higher growth should produce higher outcomes (same expense)
  expect_true(outcomes[2, 2] > outcomes[1, 2])  # 5% > 0%
  expect_true(outcomes[3, 2] > outcomes[2, 2])  # 10% > 5%

  # Lower expense should produce higher outcomes (same growth)
  expect_true(outcomes[2, 1] > outcomes[2, 2])  # 50% expense < 70%
  expect_true(outcomes[2, 2] > outcomes[2, 3])  # 70% expense < 90%
})

test_that("batch_project_growth returns expected structure", {
  skip_if_not(exists("batch_project_growth", mode = "function"),
              message = "Rcpp exports not available")

  initial_values <- c(10000, 20000, 30000, 40000, 50000)

  result <- batch_project_growth(
    initial_values = initial_values,
    growth_rate = 0.05,
    expense_rate = 0.7,
    n_periods = 12
  )

  expect_type(result, "list")
  expect_true("projections" %in% names(result))
  expect_true("final_values" %in% names(result))

  # Check dimensions
  expect_equal(nrow(result$projections), 5)
  expect_equal(ncol(result$projections), 12)
  expect_equal(length(result$final_values), 5)
})

test_that("batch_project_growth scales with initial value", {
  skip_if_not(exists("batch_project_growth", mode = "function"),
              message = "Rcpp exports not available")

  result <- batch_project_growth(
    initial_values = c(10000, 20000),
    growth_rate = 0.05,
    expense_rate = 0.7,
    n_periods = 12
  )

  # Second item (20000) should have ~2x the final value of first (10000)
  ratio <- result$final_values[2] / result$final_values[1]
  expect_true(abs(ratio - 2) < 0.1)
})

test_that("sequential_projection returns expected values", {
  skip_if_not(exists("sequential_projection", mode = "function"),
              message = "Rcpp exports not available")

  result <- sequential_projection(
    base_revenue = 100000,
    growth_rate = 0.05,
    expense_rate = 0.7,
    n_periods = 12
  )

  expect_length(result, 12)

  # Values should be increasing (positive growth with expense < 1)
  for (i in 2:12) {
    expect_true(result[i] >= result[i - 1])
  }
})

test_that("quick_sensitivity convenience function works", {
  skip_if_not(exists("parallel_sensitivity_grid", mode = "function"),
              message = "Rcpp exports not available")

  config <- list(
    entities = list(
      products = list(revenue = 50000, operating_rate = 0.65)
    ),
    growth = list(products = 0.05)
  )

  result <- quick_sensitivity(
    config,
    growth_range = c(0.00, 0.05, 0.10),
    expense_range = c(0.5, 0.7, 0.9),
    months = 6
  )

  expect_type(result, "list")
  expect_true("outcomes" %in% names(result))
})

test_that("LazyForecast collect() executes sensitivity analysis", {
  skip_if_not(exists("parallel_sensitivity_grid", mode = "function"),
              message = "Rcpp exports not available")

  config <- list(
    entities = list(
      products = list(revenue = 50000, operating_rate = 0.65)
    ),
    growth = list(products = 0.05)
  )

  lf <- forecast_expr(config) |>
    lf_sensitivity(
      growth_range = c(0.00, 0.05, 0.10),
      expense_range = c(0.5, 0.7)
    )

  result <- lf$collect()

  expect_type(result, "list")
  expect_true("outcomes" %in% names(result))
})

test_that("compare_forecasts works with multiple forecasts", {
  skip_if_not(exists("monte_carlo_parallel", mode = "function"),
              message = "Rcpp exports not available")

  config <- list(
    entities = list(
      products = list(revenue = 50000, operating_rate = 0.65)
    ),
    growth = list(products = 0.05)
  )

  base <- forecast_expr(config) |>
    lf_monte_carlo(n = 100, seed = 42)

  optimistic <- forecast_expr(config) |>
    lf_monte_carlo(n = 100, growth_mean = 0.08, seed = 42)

  comparison <- compare_forecasts(
    base = base,
    optimistic = optimistic
  )

  expect_s3_class(comparison, "tbl_df")
  expect_equal(nrow(comparison), 2)
  expect_true("scenario" %in% names(comparison))
  expect_true("mean" %in% names(comparison))

  # Optimistic should have higher mean
  base_mean <- comparison$mean[comparison$scenario == "base"]
  opt_mean <- comparison$mean[comparison$scenario == "optimistic"]
  expect_true(opt_mean > base_mean)
})
