# test-slots.R - Tests for GnuCash slots (key-value metadata) operations

test_that("gc_set_slot and gc_get_slot round-trip a string value", {
  skip_if_no_db_fixture("with-accounts.gnucash")
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(fixture_path("databases", "with-accounts.gnucash"), tmp)

  book <- gc_open(tmp, read_only = FALSE)
  on.exit(gc_close(book), add = TRUE)

  # Get an account GUID to use as obj_guid
  accounts <- gc_account_tree(book)
  guid <- accounts$guid[2]  # first non-root account

  gc_set_slot(book, guid, "test_slot", "hello")
  slot <- gc_get_slot(book, guid, "test_slot")

  expect_false(is.null(slot))
  expect_equal(slot$name, "test_slot")
  expect_equal(slot$string_val, "hello")
  expect_equal(slot$slot_type, 4L)  # STRING
})

test_that("gc_get_slot returns NULL for nonexistent slot", {
  skip_if_no_db_fixture("with-accounts.gnucash")
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(fixture_path("databases", "with-accounts.gnucash"), tmp)

  book <- gc_open(tmp, read_only = FALSE)
  on.exit(gc_close(book), add = TRUE)

  result <- gc_get_slot(book, "00000000000000000000000000000000", "no_such_slot")
  expect_null(result)
})

test_that("gc_set_slot updates existing slot value", {
  skip_if_no_db_fixture("with-accounts.gnucash")
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(fixture_path("databases", "with-accounts.gnucash"), tmp)

  book <- gc_open(tmp, read_only = FALSE)
  on.exit(gc_close(book), add = TRUE)

  accounts <- gc_account_tree(book)
  guid <- accounts$guid[2]

  gc_set_slot(book, guid, "update_test", "original")
  gc_set_slot(book, guid, "update_test", "updated")

  slot <- gc_get_slot(book, guid, "update_test")
  expect_equal(slot$string_val, "updated")
})

test_that("gc_delete_slot removes a slot", {
  skip_if_no_db_fixture("with-accounts.gnucash")
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(fixture_path("databases", "with-accounts.gnucash"), tmp)

  book <- gc_open(tmp, read_only = FALSE)
  on.exit(gc_close(book), add = TRUE)

  accounts <- gc_account_tree(book)
  guid <- accounts$guid[2]

  gc_set_slot(book, guid, "to_delete", "bye")
  gc_delete_slot(book, guid, "to_delete")

  expect_null(gc_get_slot(book, guid, "to_delete"))
})

test_that("gc_get_all_slots returns data frame", {
  skip_if_no_db_fixture("with-accounts.gnucash")
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(fixture_path("databases", "with-accounts.gnucash"), tmp)

  book <- gc_open(tmp, read_only = FALSE)
  on.exit(gc_close(book), add = TRUE)

  accounts <- gc_account_tree(book)
  guid <- accounts$guid[2]

  gc_set_slot(book, guid, "slot_a", "value_a")
  gc_set_slot(book, guid, "slot_b", "value_b")

  all_slots <- gc_get_all_slots(book, guid)
  expect_s3_class(all_slots, "data.frame")
  expect_true("name" %in% names(all_slots))
  expect_true(nrow(all_slots) >= 2)
})

test_that("gc_find_split_by_fitid returns empty for no match", {
  skip_if_no_db_fixture("with-accounts.gnucash")
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(fixture_path("databases", "with-accounts.gnucash"), tmp)

  book <- gc_open(tmp, read_only = FALSE)
  on.exit(gc_close(book), add = TRUE)

  accounts <- gc_account_tree(book)
  guid <- accounts$guid[2]

  result <- gc_find_split_by_fitid(book, guid, "NONEXISTENT_FITID")
  expect_equal(result, "")
})

test_that("gc_check_fitids returns empty data frame for no matches", {
  skip_if_no_db_fixture("with-accounts.gnucash")
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(fixture_path("databases", "with-accounts.gnucash"), tmp)

  book <- gc_open(tmp, read_only = FALSE)
  on.exit(gc_close(book), add = TRUE)

  accounts <- gc_account_tree(book)
  guid <- accounts$guid[2]

  result <- gc_check_fitids(book, guid, c("NO1", "NO2", "NO3"))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("gc_set_slot creates slots table if needed", {
  skip_if_no_db_fixture("minimal.gnucash")
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(fixture_path("databases", "minimal.gnucash"), tmp)

  book <- gc_open(tmp, read_only = FALSE)
  on.exit(gc_close(book), add = TRUE)

  # Minimal db may not have slots table; gc_set_slot should create it
  accounts <- gc_get_accounts(book)
  guid <- accounts$guid[1]

  expect_no_error(gc_set_slot(book, guid, "test_key", "test_value"))
  slot <- gc_get_slot(book, guid, "test_key")
  expect_equal(slot$string_val, "test_value")
})

test_that("multiple slots on different entities are independent", {
  skip_if_no_db_fixture("with-accounts.gnucash")
  tmp <- tempfile(fileext = ".gnucash")
  on.exit(unlink(tmp), add = TRUE)
  file.copy(fixture_path("databases", "with-accounts.gnucash"), tmp)

  book <- gc_open(tmp, read_only = FALSE)
  on.exit(gc_close(book), add = TRUE)

  accounts <- gc_account_tree(book)
  guid1 <- accounts$guid[2]
  guid2 <- accounts$guid[3]

  gc_set_slot(book, guid1, "shared_name", "value1")
  gc_set_slot(book, guid2, "shared_name", "value2")

  expect_equal(gc_get_slot(book, guid1, "shared_name")$string_val, "value1")
  expect_equal(gc_get_slot(book, guid2, "shared_name")$string_val, "value2")
})
