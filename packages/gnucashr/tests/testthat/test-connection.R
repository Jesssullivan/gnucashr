# Test GnuCash connection and basic operations

test_that("read_gnucash fails for missing file", {
  expect_error(read_gnucash("nonexistent.gnucash"))
})

test_that("GnuCashDB class exists", {
  expect_true(R6::is.R6Class(GnuCashDB))
})

test_that("BookCollection class exists", {
  expect_true(R6::is.R6Class(BookCollection))
})

test_that("detect_gnucash_format identifies formats correctly", {
  # Test with a temp SQLite file
  tmp_sqlite <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), tmp_sqlite)
  DBI::dbExecute(con, "CREATE TABLE test (id INTEGER)")
  DBI::dbDisconnect(con)

  expect_equal(detect_gnucash_format(tmp_sqlite), "sqlite")
  unlink(tmp_sqlite)

  # Test with a gzip file
  tmp_gz <- tempfile(fileext = ".gz")
  gz_con <- gzfile(tmp_gz, "wb")
  writeLines("<xml>test</xml>", gz_con)
  close(gz_con)

  expect_equal(detect_gnucash_format(tmp_gz), "xml-gz")
  unlink(tmp_gz)
})

test_that("book_collection creates empty collection", {
  collection <- book_collection()

  expect_s3_class(collection, "BookCollection")
  expect_equal(nrow(collection$list_books()), 0)

  collection$close()
})

test_that("BookCollection IC rules work", {
  collection <- book_collection()

  collection$add_ic_rule(
    "parent", "Assets:Due from Sub",
    "sub", "Liabilities:Due to Parent",
    "Test IC rule"
  )

  rules <- collection$get_ic_rules()
  expect_equal(nrow(rules), 1)
  expect_equal(rules$from_book[1], "parent")
  expect_equal(rules$to_book[1], "sub")

  collection$close()
})
