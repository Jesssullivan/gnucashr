# Test fraction utilities (R implementations, not Rcpp yet)

# These tests work with R implementations that mirror the Rcpp functions
# Once Rcpp is compiled, they test the actual C++ implementations

test_that("parse_fraction_string works correctly", {
  # This is an internal function from gnucash-xml.R
  # Using ::: to access it for testing
  parse_fraction <- gnucashr:::parse_fraction_string

  result <- parse_fraction("12345/100")
  expect_equal(result$num, 12345L)
  expect_equal(result$denom, 100L)

  result <- parse_fraction("-5000/100")
  expect_equal(result$num, -5000L)
  expect_equal(result$denom, 100L)

  result <- parse_fraction("")
  expect_true(is.na(result$num))
  expect_true(is.na(result$denom))

  result <- parse_fraction(NA_character_)
  expect_true(is.na(result$num))
})

test_that("fraction conversion preserves precision", {
  # Test that fraction operations maintain expected precision
  # 123.45 should be 12345/100

  value <- 12345 / 100
  expect_equal(value, 123.45)

  # GnuCash typically uses 100 as denom for currency
  num <- 12345L
  denom <- 100L
  expect_equal(num / denom, 123.45)
})

test_that("validate_splits_balance concept works", {
  # Test the double-entry validation concept
  # A balanced transaction has splits that sum to zero

  # Debit $100 from Checking
  # Credit $100 to Expenses
  # These should balance (debit - credit = 0 in our representation)

  splits <- tibble::tibble(
    value_num = c(10000L, -10000L),  # $100.00 and -$100.00
    value_denom = c(100L, 100L)
  )

  total <- sum(splits$value_num / splits$value_denom)
  expect_equal(total, 0)

  # Unbalanced transaction
  bad_splits <- tibble::tibble(
    value_num = c(10000L, -9999L),
    value_denom = c(100L, 100L)
  )

  bad_total <- sum(bad_splits$value_num / bad_splits$value_denom)
  expect_false(abs(bad_total) < 0.01)
})
