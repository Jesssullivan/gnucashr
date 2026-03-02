# test-reconcile.R - Tests for account reconciliation operations

test_that("gc_reconcile_account returns result structure", {
  skip_if_no_db_fixture("with-accounts.gnucash")
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(fixture_path("databases", "with-accounts.gnucash"), tmp)

  book <- gc_open(tmp, read_only = FALSE)
  on.exit(gc_close(book), add = TRUE)

  accts <- gc_account_tree(book)
  bank_accts <- accts[accts$account_type == "BANK", ]
  skip_if(nrow(bank_accts) == 0, "No bank accounts in fixture")

  result <- gc_reconcile_account(book, bank_accts$guid[1],
                                  "2024-12-31", 0.0)

  expect_type(result, "list")
  expect_true("splits_reconciled" %in% names(result))
  expect_true("statement_balance" %in% names(result))
  expect_true("book_balance" %in% names(result))
  expect_true("difference" %in% names(result))
  expect_true("balanced" %in% names(result))
})

test_that("gc_reconcile_account fails on read-only book", {
  skip_if_no_db_fixture("with-accounts.gnucash")
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(fixture_path("databases", "with-accounts.gnucash"), tmp)

  book <- gc_open(tmp, read_only = TRUE)
  on.exit(gc_close(book), add = TRUE)

  accts <- gc_account_tree(book)
  bank_accts <- accts[accts$account_type == "BANK", ]
  skip_if(nrow(bank_accts) == 0)

  expect_error(gc_reconcile_account(book, bank_accts$guid[1],
                                     "2024-12-31", 0.0),
               "read-only")
})

test_that("gc_reconcile_account with matching balance shows balanced", {
  skip_if_no_db_fixture("with-accounts.gnucash")
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(fixture_path("databases", "with-accounts.gnucash"), tmp)

  book <- gc_open(tmp, read_only = FALSE)
  on.exit(gc_close(book), add = TRUE)

  accts <- gc_account_tree(book)
  bank_accts <- accts[accts$account_type == "BANK", ]
  skip_if(nrow(bank_accts) == 0)

  # Get current balance (should be 0 for empty fixture)
  bal <- gc_get_balance(book, bank_accts$guid[1])

  result <- gc_reconcile_account(book, bank_accts$guid[1],
                                  "2024-12-31", bal)

  expect_true(result$balanced)
})

test_that("gc_find_transfer_matches returns data frame", {
  skip_if_no_db_fixture("with-accounts.gnucash")
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(fixture_path("databases", "with-accounts.gnucash"), tmp)

  book <- gc_open(tmp, read_only = TRUE)
  on.exit(gc_close(book), add = TRUE)

  accts <- gc_account_tree(book)
  bank_accts <- accts[accts$account_type == "BANK", ]
  skip_if(nrow(bank_accts) < 2, "Need at least 2 bank accounts")

  result <- gc_find_transfer_matches(book, bank_accts$guid[1],
                                      bank_accts$guid[2],
                                      "2024-01-01", "2024-12-31")

  expect_s3_class(result, "data.frame")
  expected_cols <- c("split_a_guid", "split_b_guid", "amount", "similarity")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("gc_find_transfer_matches with date window parameter", {
  skip_if_no_db_fixture("with-accounts.gnucash")
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(fixture_path("databases", "with-accounts.gnucash"), tmp)

  book <- gc_open(tmp, read_only = TRUE)
  on.exit(gc_close(book), add = TRUE)

  accts <- gc_account_tree(book)
  bank_accts <- accts[accts$account_type == "BANK", ]
  skip_if(nrow(bank_accts) < 2)

  # Narrow window should return same or fewer matches
  result_wide <- gc_find_transfer_matches(book, bank_accts$guid[1],
                                           bank_accts$guid[2],
                                           "2024-01-01", "2024-12-31",
                                           date_window = 30L)
  result_narrow <- gc_find_transfer_matches(book, bank_accts$guid[1],
                                             bank_accts$guid[2],
                                             "2024-01-01", "2024-12-31",
                                             date_window = 1L)

  expect_true(nrow(result_narrow) <= nrow(result_wide))
})
