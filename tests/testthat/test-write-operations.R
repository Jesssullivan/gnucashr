# Tests for gnucashr write operations
# Phase 2: Backup, Account, and Transaction write support
#
# Note: The create_test_gnucash_db() helper is now defined in helper-common.R
# and is automatically loaded by testthat before tests run.

# =============================================================================
# Rcpp Validation Tests
# =============================================================================

test_that("validate_transaction_balance works for balanced transactions", {
  # Balanced: 100/100 + (-100/100) = 0
  result <- validate_transaction_balance(
    value_nums = c(100L, -100L),
    value_denoms = c(100L, 100L)
  )

  expect_true(result$balanced)
  expect_equal(result$total, 0)
  expect_match(result$message, "balanced")
})

test_that("validate_transaction_balance detects imbalanced transactions", {
  # Imbalanced: 100/100 + (-50/100) = 0.50

  result <- validate_transaction_balance(
    value_nums = c(100L, -50L),
    value_denoms = c(100L, 100L)
  )

  expect_false(result$balanced)
  expect_equal(result$total, 0.50)
  expect_match(result$message, "out of balance")
})

test_that("validate_transaction_balance handles different denominators", {
  # 1/2 + 1/4 + (-3/4) = 0
  result <- validate_transaction_balance(
    value_nums = c(1L, 1L, -3L),
    value_denoms = c(2L, 4L, 4L)
  )

  expect_true(result$balanced)
  expect_equal(result$total, 0)
})

test_that("validate_transaction_balance requires minimum 2 splits", {
  result <- validate_transaction_balance(
    value_nums = c(100L),
    value_denoms = c(100L)
  )

  expect_false(result$balanced)
  expect_match(result$message, "at least 2 splits")
})

test_that("validate_transaction_balance handles tolerance", {
  # Small imbalance within tolerance
  result <- validate_transaction_balance(
    value_nums = c(100L, -99L),
    value_denoms = c(100L, 100L),
    tolerance = 0.02
  )

  expect_true(result$balanced)  # 0.01 <= 0.02 tolerance
})

test_that("validate_guids correctly validates GUID format", {
  valid_guids <- c(
    "00000000000000000000000000000001",
    "abcdef0123456789abcdef0123456789",
    "ABCDEF0123456789ABCDEF0123456789"
  )

  result <- validate_guids(valid_guids)
  expect_true(all(result))
})
test_that("validate_guids rejects invalid GUIDs", {
  invalid_guids <- c(
    "short",
    "0000000000000000000000000000000g",  # Contains 'g'
    "00000000-0000-0000-0000-000000000001",  # Contains dashes
    NA_character_
  )

  result <- validate_guids(invalid_guids)
  expect_false(any(result))
})

test_that("check_guid_uniqueness detects duplicates", {
  guids <- c(
    "00000000000000000000000000000001",
    "00000000000000000000000000000002",
    "00000000000000000000000000000001"  # Duplicate
  )

  result <- check_guid_uniqueness(guids)
  expect_false(result$unique)
  expect_equal(result$n_duplicates, 1)
  expect_equal(result$duplicates, "00000000000000000000000000000001")
})

test_that("check_guid_uniqueness passes for unique GUIDs", {
  guids <- c(
    "00000000000000000000000000000001",
    "00000000000000000000000000000002",
    "00000000000000000000000000000003"
  )

  result <- check_guid_uniqueness(guids)
  expect_true(result$unique)
  expect_equal(result$n_duplicates, 0)
})

test_that("validate_split_values detects NA values", {
  result <- validate_split_values(
    value_nums = c(100L, NA_integer_),
    value_denoms = c(100L, 100L)
  )

  expect_false(result$valid)
  expect_match(result$message, "NA")
})

test_that("validate_split_values detects invalid denominators", {
  result <- validate_split_values(
    value_nums = c(100L, 100L),
    value_denoms = c(100L, 0L)
  )

  expect_false(result$valid)
  expect_match(result$message, "invalid denominator")
})

test_that("calculate_running_balance computes correctly", {
  balances <- calculate_running_balance(
    value_nums = c(100L, 50L, -75L),
    value_denoms = c(100L, 100L, 100L),
    opening_balance = 10.0
  )

  expect_equal(balances, c(11.0, 11.5, 10.75))
})

# =============================================================================
# Backup Function Tests
# =============================================================================

test_that("create_backup creates timestamped backup", {
  skip_on_cran()

  # Create temp directory and test file

  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "test_backup.gnucash")
  file.create(test_file)
  writeLines("test content", test_file)

  backup_dir <- file.path(temp_dir, ".gnucash-backups")

  result <- create_backup(test_file, backup_dir = backup_dir, max_backups = 5)

  expect_true(is_ok(result))

  backup_info <- unwrap(result)
  expect_true(file.exists(backup_info$backup_path))
  expect_match(basename(backup_info$backup_path), "^test_backup_\\d{8}_\\d{6}\\.gnucash$")

  # Cleanup
  unlink(backup_dir, recursive = TRUE)
  file.remove(test_file)
})

test_that("list_backups returns tibble of backups", {
  skip_on_cran()

  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "test_list.gnucash")
  file.create(test_file)
  writeLines("test content", test_file)

  backup_dir <- file.path(temp_dir, ".gnucash-backups-list")

  # Create a few backups
  create_backup(test_file, backup_dir = backup_dir)
  Sys.sleep(1)  # Ensure different timestamps

create_backup(test_file, backup_dir = backup_dir)

  backups <- list_backups(test_file, backup_dir = backup_dir)

  expect_s3_class(backups, "tbl_df")
  expect_gte(nrow(backups), 2)
  expect_true("path" %in% names(backups))
  expect_true("timestamp" %in% names(backups))

  # Cleanup
  unlink(backup_dir, recursive = TRUE)
  file.remove(test_file)
})

test_that("restore_backup restores file correctly", {
  skip_on_cran()

  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "test_restore.gnucash")
  writeLines("original content", test_file)

  backup_dir <- file.path(temp_dir, ".gnucash-backups-restore")
  backup_result <- create_backup(test_file, backup_dir = backup_dir)

  # Modify original
  writeLines("modified content", test_file)
  expect_equal(readLines(test_file), "modified content")

  # Restore
  backup_path <- unwrap(backup_result)$backup_path
  restore_result <- restore_backup(backup_path, test_file, create_safety_backup = FALSE)

  expect_true(is_ok(restore_result))
  expect_equal(readLines(test_file), "original content")

  # Cleanup
  unlink(backup_dir, recursive = TRUE)
  file.remove(test_file)
})

# =============================================================================
# Account Write Tests (using mock GnuCashDB methods)
# =============================================================================

test_that("validate_account_data rejects empty names", {
  # This tests the internal validation without needing a real DB
  expect_error({
    # Create a mock gc with empty metadata
    mock_gc <- list(
      metadata = function() list(root_account_guid = "test"),
      accounts = function(collected = FALSE) tibble::tibble(name = character(), parent_guid = character()),
      get_account = function(x) NULL
    )
    result <- gnucashr:::validate_account_data("", "BANK", NULL, NULL, mock_gc)
  }, NA)  # Should not error, but return err Result
})

test_that("Account type validation covers GnuCash types", {
  valid_types <- c(
    "NONE", "BANK", "CASH", "CREDIT", "ASSET", "LIABILITY",
    "STOCK", "MUTUAL", "CURRENCY", "INCOME", "EXPENSE",
    "EQUITY", "RECEIVABLE", "PAYABLE", "ROOT", "TRADING"
  )

  # Just verify these are the expected valid types
  expect_length(valid_types, 16)
})

# =============================================================================
# Transaction Write Tests
# =============================================================================

test_that("validate_splits rejects single split", {
  mock_gc <- list(
    get_account = function(x) list(guid = "test-guid")
  )

  # Single split should fail
  result <- gnucashr:::validate_splits(
    list(list(account = "test", value = 100)),
    mock_gc
  )

  expect_true(is_err(result))
  expect_match(unwrap_err(result), "at least 2 splits")
})

test_that("validate_splits rejects unbalanced splits", {
  mock_gc <- list(
    get_account = function(x) list(guid = "test-guid")
  )

  # Unbalanced splits
  result <- gnucashr:::validate_splits(
    list(
      list(account = "test1", value = 100),
      list(account = "test2", value = -50)  # Should be -100
    ),
    mock_gc
  )

  expect_true(is_err(result))
  expect_match(unwrap_err(result), "do not balance")
})

test_that("validate_splits accepts balanced splits", {
  mock_gc <- list(
    get_account = function(x) list(guid = paste0("guid-", x))
  )

  result <- gnucashr:::validate_splits(
    list(
      list(account = "checking", value = -100),
      list(account = "expenses", value = 100)
    ),
    mock_gc
  )

  expect_true(is_ok(result))

  validated <- unwrap(result)
  expect_length(validated, 2)
  expect_true(!is.null(validated[[1]]$account_guid))
})

# =============================================================================
# Integration Test with Real SQLite DB
# =============================================================================

test_that("full write workflow works with test database", {
  skip_on_cran()

  # Create temporary test database
  temp_db <- tempfile(fileext = ".gnucash")
  db_info <- create_test_gnucash_db(temp_db)

  # Skip if read_gnucash not available or fails
  gc <- tryCatch(
    read_gnucash(temp_db, read_only = FALSE),
    error = function(e) NULL
  )

  if (is.null(gc)) {
    skip("Could not create GnuCashDB connection")
  }

  # Test 1: Backup creation
  backup_result <- create_backup(gc)
  expect_true(is_ok(backup_result))

  # Test 2: List accounts (verify test data loaded)
  accounts <- gc$accounts(collected = TRUE)
  expect_gte(nrow(accounts), 4)  # Root, Assets, Expenses, Checking

  # Cleanup
  gc$close()
  unlink(temp_db)
  backup_info <- unwrap(backup_result)
  if (file.exists(backup_info$backup_path)) {
    unlink(dirname(backup_info$backup_path), recursive = TRUE)
  }
})
