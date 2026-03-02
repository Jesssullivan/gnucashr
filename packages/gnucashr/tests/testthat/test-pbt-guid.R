# test-pbt-guid.R - Property-based tests for GUID generation and validation
#
# Uses hedgehog for PBT to verify GUID properties:
# - Format: Always 32 hex characters
# - Uniqueness: generate_guids(n) produces n unique values
# - Validation consistency

test_that("generate_guid produces 32-character hex strings", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("generate_guid", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  # Property: every generated GUID is 32 hex chars
  forall(
    list(ignored = gen.int(100)),  # Just for iteration
    function(ignored) {
      guid <- generate_guid()

      # Check length
      expect_equal(nchar(guid), 32)

      # Check all characters are hex digits
      expect_true(grepl("^[0-9a-f]{32}$", guid))
    }
  )
})

test_that("generate_guids(n) produces exactly n GUIDs", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("generate_guids", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(n = gen.element(c(1L, 5L, 10L, 50L, 100L))),
    function(n) {
      guids <- generate_guids(n)
      expect_equal(length(guids), n)
    }
  )
})

test_that("generate_guids produces unique values", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("generate_guids", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(n = gen.element(c(10L, 50L, 100L, 500L))),
    function(n) {
      guids <- generate_guids(n)

      # All GUIDs should be unique
      expect_equal(length(unique(guids)), n)
    }
  )
})

test_that("all generated GUIDs pass validate_guid", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("generate_guids", mode = "function"),
              message = "Rcpp exports not available")
  skip_if_not(exists("validate_guid", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(n = gen.element(c(1L, 10L, 50L))),
    function(n) {
      guids <- generate_guids(n)

      for (guid in guids) {
        expect_true(validate_guid(guid))
      }
    }
  )
})

test_that("all generated GUIDs pass validate_guids (vectorized)", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("generate_guids", mode = "function"),
              message = "Rcpp exports not available")
  skip_if_not(exists("validate_guids", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(n = gen.element(c(10L, 50L, 100L))),
    function(n) {
      guids <- generate_guids(n)
      valid <- validate_guids(guids)

      expect_true(all(valid))
    }
  )
})

test_that("check_guid_uniqueness correctly identifies unique batch", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("generate_guids", mode = "function"),
              message = "Rcpp exports not available")
  skip_if_not(exists("check_guid_uniqueness", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(n = gen.element(c(10L, 50L, 100L))),
    function(n) {
      guids <- generate_guids(n)
      result <- check_guid_uniqueness(guids)

      expect_true(result$unique)
      expect_equal(result$n_duplicates, 0)
      expect_equal(length(result$duplicates), 0)
    }
  )
})

test_that("validate_guid rejects invalid formats", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("validate_guid", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  # Wrong length strings
  forall(
    list(len = gen.element(c(1L, 16L, 31L, 33L, 64L))),
    function(len) {
      invalid <- paste(rep("a", len), collapse = "")
      expect_false(validate_guid(invalid))
    }
  )
})

test_that("validate_guid rejects non-hex characters", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("validate_guid", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(
      pos = gen.element(1L:32L),
      bad_char = gen.element(c("g", "z", "G", "Z", "!", "@", " "))
    ),
    function(pos, bad_char) {
      # Create a valid base and insert invalid char
      base <- paste(rep("a", 32), collapse = "")
      invalid <- paste0(
        substr(base, 1, pos - 1),
        bad_char,
        substr(base, pos + 1, 32)
      )

      expect_false(validate_guid(invalid))
    }
  )
})

test_that("validate_guids returns correct length result", {
  skip_if_not_installed("hedgehog")
  skip_if_not(exists("validate_guids", mode = "function"),
              message = "Rcpp exports not available")

  library(hedgehog)

  forall(
    list(n = gen.element(c(1L, 10L, 50L, 100L))),
    function(n) {
      # Mix of valid and invalid GUIDs
      valid_guids <- generate_guids(n %/% 2L)
      invalid_guids <- rep("invalid", n - n %/% 2L)
      all_guids <- c(valid_guids, invalid_guids)

      result <- validate_guids(all_guids)

      expect_equal(length(result), length(all_guids))
    }
  )
})
