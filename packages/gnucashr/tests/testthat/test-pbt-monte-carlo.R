# test-pbt-monte-carlo.R - Property-based tests for Monte Carlo simulation
#
# Uses hedgehog for PBT to verify Monte Carlo properties:
# - Reproducibility: Same seed = identical results
# - Quantile ordering: p5 <= p25 <= p50 <= p75 <= p95
# - Statistical consistency

test_that("monte_carlo_parallel is reproducible with same seed", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("monte_carlo_parallel", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      seed = gen.element(c(1L, 42L, 123L, 999L, 12345L)),
      base_revenue = gen.unif(10000, 500000),
      expense_rate = gen.unif(0.3, 0.9)
    ),
    function(seed, base_revenue, expense_rate) {
      result1 <- monte_carlo_parallel(
        base_revenue = base_revenue,
        base_expense_rate = expense_rate,
        n_sims = 100,
        n_periods = 6,
        seed = seed
      )

      result2 <- monte_carlo_parallel(
        base_revenue = base_revenue,
        base_expense_rate = expense_rate,
        n_sims = 100,
        n_periods = 6,
        seed = seed
      )

      # Results should be identical
      expect_equal(result1$final_cash, result2$final_cash)
      expect_equal(result1$summary$mean, result2$summary$mean)
    }
  )
})

test_that("monte_carlo_parallel produces different results with different seeds", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("monte_carlo_parallel", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      seed1 = gen.int(10000),
      base_revenue = gen.unif(50000, 200000)
    ),
    function(seed1, base_revenue) {
      seed2 <- seed1 + 1L  # Ensure different seed

      result1 <- monte_carlo_parallel(
        base_revenue = base_revenue,
        base_expense_rate = 0.7,
        n_sims = 100,
        seed = seed1
      )

      result2 <- monte_carlo_parallel(
        base_revenue = base_revenue,
        base_expense_rate = 0.7,
        n_sims = 100,
        seed = seed2
      )

      # Results should differ
      expect_false(all(result1$final_cash == result2$final_cash))
    }
  )
})

test_that("monte_carlo_parallel quantiles are always ordered correctly", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("monte_carlo_parallel", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      base_revenue = gen.unif(10000, 500000),
      expense_rate = gen.unif(0.2, 0.9),
      growth_mean = gen.unif(-0.1, 0.2),
      growth_sd = gen.unif(0.01, 0.1),
      seed = gen.int(10000)
    ),
    function(base_revenue, expense_rate, growth_mean, growth_sd, seed) {
      result <- monte_carlo_parallel(
        base_revenue = base_revenue,
        base_expense_rate = expense_rate,
        n_sims = 500,
        n_periods = 12,
        growth_mean = growth_mean,
        growth_sd = growth_sd,
        seed = seed
      )

      summary <- result$summary

      # Quantiles must be ordered
      expect_true(summary$min <= summary$p5)
      expect_true(summary$p5 <= summary$p10)
      expect_true(summary$p10 <= summary$p25)
      expect_true(summary$p25 <= summary$p50)
      expect_true(summary$p50 <= summary$p75)
      expect_true(summary$p75 <= summary$p90)
      expect_true(summary$p90 <= summary$p95)
      expect_true(summary$p95 <= summary$max)
    }
  )
})

test_that("monte_carlo_parallel returns correct dimensions", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("monte_carlo_parallel", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      n_sims = gen.element(c(50L, 100L, 500L)),
      n_periods = gen.element(c(6L, 12L, 24L))
    ),
    function(n_sims, n_periods) {
      result <- monte_carlo_parallel(
        base_revenue = 100000,
        base_expense_rate = 0.7,
        n_sims = n_sims,
        n_periods = n_periods,
        seed = 42
      )

      expect_equal(nrow(result$results), n_sims)
      expect_equal(ncol(result$results), n_periods)
      expect_equal(length(result$final_cash), n_sims)
    }
  )
})

test_that("monte_carlo_parallel mean is within reasonable range of median", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("monte_carlo_parallel", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      base_revenue = gen.unif(50000, 200000),
      seed = gen.int(10000)
    ),
    function(base_revenue, seed) {
      # Using moderate growth and SD for roughly normal distribution
      result <- monte_carlo_parallel(
        base_revenue = base_revenue,
        base_expense_rate = 0.7,
        n_sims = 1000,
        n_periods = 12,
        growth_mean = 0.03,
        growth_sd = 0.02,
        seed = seed
      )

      summary <- result$summary

      # Mean and median should be relatively close for normal-ish distributions
      # Allow 50% relative difference to account for skewness
      if (summary$p50 != 0) {
        relative_diff <- abs(summary$mean - summary$p50) / abs(summary$p50)
        expect_true(relative_diff < 0.5)
      }
    }
  )
})

test_that("monte_carlo_parallel standard deviation is non-negative", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("monte_carlo_parallel", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      base_revenue = gen.unif(10000, 500000),
      growth_sd = gen.unif(0.001, 0.2),
      seed = gen.int(10000)
    ),
    function(base_revenue, growth_sd, seed) {
      result <- monte_carlo_parallel(
        base_revenue = base_revenue,
        base_expense_rate = 0.7,
        n_sims = 500,
        growth_sd = growth_sd,
        seed = seed
      )

      expect_true(result$summary$sd >= 0)
    }
  )
})

test_that("monte_carlo_multi_entity is reproducible with same seed", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("monte_carlo_multi_entity", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      seed = gen.element(c(1L, 42L, 123L, 999L))
    ),
    function(seed) {
      base_revenues <- c(50000, 70000, 30000)
      growth_means <- c(0.05, 0.08, 0.03)
      growth_sds <- c(0.02, 0.03, 0.01)
      expense_rates <- c(0.65, 0.70, 0.50)

      result1 <- monte_carlo_multi_entity(
        base_revenues = base_revenues,
        growth_means = growth_means,
        growth_sds = growth_sds,
        expense_rates = expense_rates,
        n_sims = 100,
        n_periods = 6,
        seed = seed
      )

      result2 <- monte_carlo_multi_entity(
        base_revenues = base_revenues,
        growth_means = growth_means,
        growth_sds = growth_sds,
        expense_rates = expense_rates,
        n_sims = 100,
        n_periods = 6,
        seed = seed
      )

      expect_equal(result1$total_cash, result2$total_cash)
    }
  )
})

test_that("monte_carlo_multi_entity quantiles are ordered", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("monte_carlo_multi_entity", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      n_entities = gen.element(c(2L, 3L, 5L)),
      seed = gen.int(10000)
    ),
    function(n_entities, seed) {
      base_revenues <- rep(50000, n_entities)
      growth_means <- rep(0.05, n_entities)
      growth_sds <- rep(0.02, n_entities)
      expense_rates <- rep(0.70, n_entities)

      result <- monte_carlo_multi_entity(
        base_revenues = base_revenues,
        growth_means = growth_means,
        growth_sds = growth_sds,
        expense_rates = expense_rates,
        n_sims = 500,
        n_periods = 12,
        seed = seed
      )

      summary <- result$summary

      expect_true(summary$p5 <= summary$p25)
      expect_true(summary$p25 <= summary$p50)
      expect_true(summary$p50 <= summary$p75)
      expect_true(summary$p75 <= summary$p95)
    }
  )
})

test_that("monte_carlo_multi_entity returns correct dimensions", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("monte_carlo_multi_entity", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      n_entities = gen.element(c(2L, 3L, 5L)),
      n_sims = gen.element(c(50L, 100L, 200L)),
      n_periods = gen.element(c(6L, 12L))
    ),
    function(n_entities, n_sims, n_periods) {
      base_revenues <- rep(50000, n_entities)
      growth_means <- rep(0.05, n_entities)
      growth_sds <- rep(0.02, n_entities)
      expense_rates <- rep(0.70, n_entities)

      result <- monte_carlo_multi_entity(
        base_revenues = base_revenues,
        growth_means = growth_means,
        growth_sds = growth_sds,
        expense_rates = expense_rates,
        n_sims = n_sims,
        n_periods = n_periods,
        seed = 42
      )

      expect_equal(nrow(result$entity_results), n_sims)
      expect_equal(ncol(result$entity_results), n_entities * n_periods)
      expect_equal(length(result$total_cash), n_sims)
    }
  )
})
