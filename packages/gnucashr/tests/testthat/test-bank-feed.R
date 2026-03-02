# test-bank-feed.R - Tests for bank feed import operations

test_that("gc_import_csv_feed imports transactions into book", {
  skip_if_no_db_fixture("with-accounts.gnucash")
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(fixture_path("databases", "with-accounts.gnucash"), tmp)

  book <- gc_open(tmp, read_only = FALSE)
  on.exit(gc_close(book), add = TRUE)

  # Find bank and expense accounts
  accts <- gc_account_tree(book)
  bank_accts <- accts[accts$account_type == "BANK", ]
  expense_accts <- accts[accts$account_type == "EXPENSE", ]

  skip_if(nrow(bank_accts) == 0, "No bank accounts in fixture")
  skip_if(nrow(expense_accts) == 0, "No expense accounts in fixture")

  bank_guid <- bank_accts$guid[1]
  expense_guid <- expense_accts$guid[1]

  csv_content <- "date,amount,description\n2024-03-01,-25.50,Coffee Shop\n2024-03-02,-100.00,Grocery Store\n"

  result <- gc_import_csv_feed(book, csv_content, "generic", bank_guid, expense_guid)

  expect_equal(result$total_parsed, 2)
  expect_equal(result$imported, 2)
  expect_equal(result$duplicates, 0)
  expect_equal(length(result$imported_guids), 2)
})

test_that("gc_import_csv_feed deduplicates on reimport", {
  skip_if_no_db_fixture("with-accounts.gnucash")
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(fixture_path("databases", "with-accounts.gnucash"), tmp)

  book <- gc_open(tmp, read_only = FALSE)
  on.exit(gc_close(book), add = TRUE)

  accts <- gc_account_tree(book)
  bank_accts <- accts[accts$account_type == "BANK", ]
  expense_accts <- accts[accts$account_type == "EXPENSE", ]

  skip_if(nrow(bank_accts) == 0 || nrow(expense_accts) == 0)

  bank_guid <- bank_accts$guid[1]
  expense_guid <- expense_accts$guid[1]

  csv_content <- "date,amount,description\n2024-04-01,-15.00,Dedup Test\n"

  # Import once
  r1 <- gc_import_csv_feed(book, csv_content, "generic", bank_guid, expense_guid)
  expect_equal(r1$imported, 1)

  # Import again - should be duplicate
  r2 <- gc_import_csv_feed(book, csv_content, "generic", bank_guid, expense_guid)
  expect_equal(r2$imported, 0)
  expect_equal(r2$duplicates, 1)
})

test_that("gc_check_duplicates returns matching fitids", {
  skip_if_no_db_fixture("with-accounts.gnucash")
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(fixture_path("databases", "with-accounts.gnucash"), tmp)

  book <- gc_open(tmp, read_only = FALSE)
  on.exit(gc_close(book), add = TRUE)

  accts <- gc_account_tree(book)
  bank_accts <- accts[accts$account_type == "BANK", ]
  expense_accts <- accts[accts$account_type == "EXPENSE", ]
  skip_if(nrow(bank_accts) == 0 || nrow(expense_accts) == 0)

  bank_guid <- bank_accts$guid[1]
  expense_guid <- expense_accts$guid[1]

  # Import a transaction so it gets an FITID
  csv_content <- "date,amount,description\n2024-05-01,-20.00,Check Dup Test\n"
  gc_import_csv_feed(book, csv_content, "generic", bank_guid, expense_guid)

  # Check for nonexistent FITIDs
  result <- gc_check_duplicates(book, bank_guid, c("NONEXISTENT1", "NONEXISTENT2"))
  expect_s3_class(result, "data.frame")
})

test_that("gc_import_csv_feed handles empty CSV", {
  skip_if_no_db_fixture("with-accounts.gnucash")
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(fixture_path("databases", "with-accounts.gnucash"), tmp)

  book <- gc_open(tmp, read_only = FALSE)
  on.exit(gc_close(book), add = TRUE)

  accts <- gc_account_tree(book)
  bank_accts <- accts[accts$account_type == "BANK", ]
  expense_accts <- accts[accts$account_type == "EXPENSE", ]
  skip_if(nrow(bank_accts) == 0 || nrow(expense_accts) == 0)

  csv_content <- "date,amount,description\n"
  result <- gc_import_csv_feed(book, csv_content, "generic",
                                bank_accts$guid[1], expense_accts$guid[1])
  expect_equal(result$total_parsed, 0)
  expect_equal(result$imported, 0)
})
