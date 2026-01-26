# test-pbt-fractions.R - Property-based tests for fraction arithmetic
#
# Uses hedgehog for PBT to verify mathematical properties:
# - Commutativity of addition
# - Associativity of addition
# - Round-trip conversion properties
# - Reduction to lowest terms

test_that("fraction_to_double is consistent with R division", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("fraction_to_double", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      num = gen.int(10000),
      denom = gen.element(c(1L, 10L, 100L, 1000L))
    ),
    function(num, denom) {
      rcpp_result <- fraction_to_double(num, denom)
      r_result <- num / denom
      expect_equal(rcpp_result, r_result, tolerance = 1e-10)
    }
  )
})

test_that("fraction_to_double handles negative numerators", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("fraction_to_double", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      num = gen.int(10000),
      denom = gen.element(c(1L, 10L, 100L, 1000L))
    ),
    function(num, denom) {
      neg_num <- -abs(num)
      rcpp_result <- fraction_to_double(neg_num, denom)
      r_result <- neg_num / denom
      expect_equal(rcpp_result, r_result, tolerance = 1e-10)
    }
  )
})

test_that("fraction addition is commutative: a + b == b + a", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("add_fractions", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      n1 = gen.int(10000),
      d1 = gen.element(c(100L, 1000L)),
      n2 = gen.int(10000),
      d2 = gen.element(c(100L, 1000L))
    ),
    function(n1, d1, n2, d2) {
      # a + b
      result_ab <- add_fractions(n1, d1, n2, d2)
      # b + a
      result_ba <- add_fractions(n2, d2, n1, d1)

      # Convert to double for comparison (reduction may differ but value equal)
      val_ab <- result_ab[1, "numerator"] / result_ab[1, "denominator"]
      val_ba <- result_ba[1, "numerator"] / result_ba[1, "denominator"]

      expect_equal(val_ab, val_ba, tolerance = 1e-10)
    }
  )
})

test_that("fraction addition is associative: (a + b) + c == a + (b + c)", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("add_fractions", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      n1 = gen.int(1000),
      d1 = gen.element(c(100L)),
      n2 = gen.int(1000),
      d2 = gen.element(c(100L)),
      n3 = gen.int(1000),
      d3 = gen.element(c(100L))
    ),
    function(n1, d1, n2, d2, n3, d3) {
      # (a + b) + c
      ab <- add_fractions(n1, d1, n2, d2)
      ab_c <- add_fractions(
        as.integer(ab[1, "numerator"]),
        as.integer(ab[1, "denominator"]),
        n3, d3
      )

      # a + (b + c)
      bc <- add_fractions(n2, d2, n3, d3)
      a_bc <- add_fractions(
        n1, d1,
        as.integer(bc[1, "numerator"]),
        as.integer(bc[1, "denominator"])
      )

      # Compare values
      val_abc <- ab_c[1, "numerator"] / ab_c[1, "denominator"]
      val_abc2 <- a_bc[1, "numerator"] / a_bc[1, "denominator"]

      expect_equal(val_abc, val_abc2, tolerance = 1e-10)
    }
  )
})

test_that("fraction addition has identity: a + 0 == a", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("add_fractions", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      num = gen.int(10000),
      denom = gen.element(c(100L, 1000L))
    ),
    function(num, denom) {
      # a + 0
      result <- add_fractions(num, denom, 0L, 1L)

      val_original <- num / denom
      val_result <- result[1, "numerator"] / result[1, "denominator"]

      expect_equal(val_original, val_result, tolerance = 1e-10)
    }
  )
})

test_that("fraction addition inverse: a + (-a) == 0", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("add_fractions", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      num = gen.int(10000),
      denom = gen.element(c(100L, 1000L))
    ),
    function(num, denom) {
      neg_num <- -num

      result <- add_fractions(num, denom, neg_num, denom)

      # Should equal zero
      val_result <- result[1, "numerator"] / result[1, "denominator"]
      expect_equal(val_result, 0, tolerance = 1e-10)
    }
  )
})

test_that("double_to_fraction round-trip preserves value within precision", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("double_to_fraction", mode = "function"),
              message = "Rcpp exports not available")
  skip_if_not(exists("fraction_to_double", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      value = gen.unif(-10000, 10000),
      denom = gen.element(c(100L, 1000L, 10000L))
    ),
    function(value, denom) {
      # Convert double to fraction
      frac <- double_to_fraction(value, denom)
      num <- frac[1, "numerator"]
      denom_out <- frac[1, "denominator"]

      # Convert back to double
      recovered <- fraction_to_double(num, denom_out)

      # Should be within 1/denom precision
      tolerance <- 1.0 / denom
      expect_equal(recovered, value, tolerance = tolerance)
    }
  )
})

test_that("fraction_to_double * denominator approximately equals numerator", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("fraction_to_double", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      num = gen.int(100000),
      denom = gen.element(c(1L, 10L, 100L, 1000L))
    ),
    function(num, denom) {
      result <- fraction_to_double(num, denom)

      # result * denom should approximately equal num
      # (exact for integers, may have small floating point error)
      expect_equal(result * denom, num, tolerance = 1e-9)
    }
  )
})

test_that("validate_splits_balance correctly identifies balanced splits", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("validate_splits_balance", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      amount1 = gen.int(100000),
      amount2 = gen.int(100000)
    ),
    function(amount1, amount2) {
      # Create balanced splits: debit and credit
      nums <- c(amount1, amount2, -(amount1 + amount2))
      denoms <- c(100L, 100L, 100L)

      # Should always be balanced
      result <- validate_splits_balance(nums, denoms)
      expect_true(result)
    }
  )
})

test_that("validate_splits_balance correctly identifies unbalanced splits", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("validate_splits_balance", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      amount1 = gen.int(100000),
      amount2 = gen.int(100000),
      imbalance = gen.element(c(1L, -1L, 10L, -10L, 100L))
    ),
    function(amount1, amount2, imbalance) {
      # Create unbalanced splits with small imbalance
      nums <- c(amount1, amount2, -(amount1 + amount2) + imbalance)
      denoms <- c(100L, 100L, 100L)

      # Should be unbalanced
      result <- validate_splits_balance(nums, denoms)
      expect_false(result)
    }
  )
})

test_that("add_fractions produces valid fraction (non-zero denominator)", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("add_fractions", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      n1 = gen.int(10000),
      d1 = gen.element(c(1L, 10L, 100L, 1000L)),
      n2 = gen.int(10000),
      d2 = gen.element(c(1L, 10L, 100L, 1000L))
    ),
    function(n1, d1, n2, d2) {
      result <- add_fractions(n1, d1, n2, d2)

      # Denominator should always be positive and non-zero
      expect_true(result[1, "denominator"] > 0)
    }
  )
})

test_that("fraction operations are consistent across different denominators", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("add_fractions", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      value1 = gen.int(1000),
      value2 = gen.int(1000)
    ),
    function(value1, value2) {
      # Same mathematical value with different denominators
      # value1/100 = (value1*10)/1000
      result1 <- add_fractions(value1, 100L, value2, 100L)
      result2 <- add_fractions(value1 * 10L, 1000L, value2 * 10L, 1000L)

      val1 <- result1[1, "numerator"] / result1[1, "denominator"]
      val2 <- result2[1, "numerator"] / result2[1, "denominator"]

      expect_equal(val1, val2, tolerance = 1e-10)
    }
  )
})
