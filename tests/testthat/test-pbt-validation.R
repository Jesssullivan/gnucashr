# test-pbt-validation.R - Property-based tests for transaction validation
#
# Uses hedgehog for PBT to verify validation properties:
# - Balance: Sum-zero splits always validate as balanced
# - Consistency of validation functions

test_that("validate_transaction_balance accepts balanced splits", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("validate_transaction_balance", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      amount1 = gen.int(100000),
      amount2 = gen.int(100000)
    ),
    function(amount1, amount2) {
      # Create balanced splits: two positive, one negative that balances
      nums <- c(amount1, amount2, -(amount1 + amount2))
      denoms <- c(100L, 100L, 100L)

      result <- validate_transaction_balance(nums, denoms)

      expect_true(result$balanced)
      expect_equal(result$total, 0, tolerance = 1e-10)
    }
  )
})

test_that("validate_transaction_balance rejects unbalanced splits", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("validate_transaction_balance", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      amount1 = gen.int(100000),
      amount2 = gen.int(100000),
      imbalance = gen.element(c(1L, -1L, 100L, -100L, 1000L))
    ),
    function(amount1, amount2, imbalance) {
      # Create unbalanced splits
      nums <- c(amount1, amount2, -(amount1 + amount2) + imbalance)
      denoms <- c(100L, 100L, 100L)

      result <- validate_transaction_balance(nums, denoms, tolerance = 0.0)

      expect_false(result$balanced)
      expect_true(abs(result$total) > 0)
    }
  )
})

test_that("validate_transaction_balance works with mixed denominators", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("validate_transaction_balance", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      amount = gen.int(10000)
    ),
    function(amount) {
      # 50/100 + 500/1000 - 1000/1000 should balance
      # 0.50 + 0.50 - 1.00 = 0
      # Use the property: equivalent fractions should balance
      nums <- c(amount, amount * 10L, -(amount * 2L * 10L))
      denoms <- c(100L, 1000L, 1000L)

      result <- validate_transaction_balance(nums, denoms)

      expect_true(result$balanced)
    }
  )
})

test_that("validate_splits_balance consistent with validate_transaction_balance", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("validate_splits_balance", mode = "function"),
              message = "Rcpp exports not available")
  skip_if_not(exists("validate_transaction_balance", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      amount1 = gen.int(50000),
      amount2 = gen.int(50000)
    ),
    function(amount1, amount2) {
      # Balanced case
      nums <- c(amount1, amount2, -(amount1 + amount2))
      denoms <- c(100L, 100L, 100L)

      result1 <- validate_splits_balance(nums, denoms)
      result2 <- validate_transaction_balance(nums, denoms)

      # Both should agree on balanced status
      expect_equal(result1, result2$balanced)
    }
  )
})

test_that("validate_split_values accepts valid splits", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("validate_split_values", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      n = gen.element(c(2L, 3L, 5L, 10L))
    ),
    function(n) {
      nums <- sample(c(-10000:10000), n)
      denoms <- rep(100L, n)

      result <- validate_split_values(nums, denoms)

      expect_true(result$valid)
    }
  )
})

test_that("validate_split_values rejects invalid denominators", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("validate_split_values", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      n = gen.element(c(2L, 5L, 10L)),
      bad_idx = gen.int(10)
    ),
    function(n, bad_idx) {
      idx <- ((abs(bad_idx) - 1L) %% n) + 1L

      nums <- sample(c(1000:10000), n)
      denoms <- rep(100L, n)

      # Insert invalid denominator (zero or negative)
      denoms[idx] <- 0L

      result <- validate_split_values(nums, denoms)

      expect_false(result$valid)
      expect_true(idx %in% result$invalid_denom_indices)
    }
  )
})

test_that("calculate_running_balance is monotonic with same sign inputs", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("calculate_running_balance", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      n = gen.element(c(5L, 10L, 20L))
    ),
    function(n) {
      # All positive values should give monotonically increasing balance
      nums <- sample(100L:1000L, n, replace = TRUE)
      denoms <- rep(100L, n)

      balances <- calculate_running_balance(nums, denoms, 0.0)

      # Each balance should be >= previous
      for (i in 2:n) {
        expect_true(balances[i] >= balances[i - 1])
      }
    }
  )
})

test_that("calculate_running_balance final value equals sum", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("calculate_running_balance", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      n = gen.element(c(5L, 10L, 20L)),
      opening = gen.unif(-10000, 10000)
    ),
    function(n, opening) {
      nums <- sample(-5000L:5000L, n, replace = TRUE)
      denoms <- rep(100L, n)

      balances <- calculate_running_balance(nums, denoms, opening)

      # Final balance should equal opening + sum of all values
      expected_final <- opening + sum(nums / denoms)
      expect_equal(balances[n], expected_final, tolerance = 1e-10)
    }
  )
})

test_that("validate_guids validates all generated GUIDs", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("validate_guids", mode = "function"),
              message = "Rcpp exports not available")
  skip_if_not(exists("generate_guids", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(n = gen.element(c(10L, 50L, 100L))),
    function(n) {
      guids <- generate_guids(n)
      valid <- validate_guids(guids)

      expect_true(all(valid))
      expect_equal(length(valid), n)
    }
  )
})

test_that("check_guid_uniqueness finds duplicates", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("check_guid_uniqueness", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(n = gen.element(c(5L, 10L, 20L))),
    function(n) {
      # Create list with intentional duplicate
      guids <- generate_guids(n)
      guids_with_dup <- c(guids, guids[1])  # Add duplicate of first

      result <- check_guid_uniqueness(guids_with_dup)

      expect_false(result$unique)
      expect_equal(result$n_duplicates, 1)
      expect_true(guids[1] %in% result$duplicates)
    }
  )
})

test_that("validate_transaction_balance handles tolerance correctly", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("validate_transaction_balance", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      amount = gen.int(100000),
      small_imbalance = gen.element(c(1L, 2L, 5L))
    ),
    function(amount, small_imbalance) {
      # Create slightly unbalanced transaction
      nums <- c(amount, -amount + small_imbalance)
      denoms <- c(100L, 100L)

      # Should fail with zero tolerance
      result_strict <- validate_transaction_balance(nums, denoms, tolerance = 0.0)
      expect_false(result_strict$balanced)

      # Should pass with sufficient tolerance
      tolerance <- small_imbalance / 100.0 + 0.01
      result_tolerant <- validate_transaction_balance(nums, denoms, tolerance = tolerance)
      expect_true(result_tolerant$balanced)
    }
  )
})
