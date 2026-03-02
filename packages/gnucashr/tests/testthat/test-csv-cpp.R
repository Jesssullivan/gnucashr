# test-csv-cpp.R - Tests for C++ CSV parser

test_that("gc_parse_csv parses generic CSV format", {
  csv_content <- "date,amount,description\n2024-01-15,45.23,Grocery Store\n2024-01-16,-10.50,Coffee Shop\n"
  result <- gc_parse_csv(csv_content, "generic")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$date[1], "2024-01-15")
  expect_equal(result$description[1], "Grocery Store")
  expect_equal(result$amount_num[1], 4523)
  expect_equal(result$amount_denom[1], 100)
})

test_that("gc_parse_csv handles negative amounts", {
  csv_content <- "date,amount,description\n2024-01-15,-99.99,Refund\n"
  result <- gc_parse_csv(csv_content, "generic")

  expect_equal(nrow(result), 1)
  expect_equal(result$amount_num[1], -9999)
  expect_equal(result$amount_denom[1], 100)
})

test_that("gc_parse_csv handles quoted fields with commas", {
  csv_content <- "date,amount,description\n2024-01-15,50.00,\"Smith, John\"\n"
  result <- gc_parse_csv(csv_content, "generic")

  expect_equal(nrow(result), 1)
  expect_equal(result$description[1], "Smith, John")
})

test_that("gc_parse_csv returns all expected columns", {
  csv_content <- "date,amount,description\n2024-01-01,1.00,Test\n"
  result <- gc_parse_csv(csv_content, "generic")

  expected_cols <- c("date", "amount_num", "amount_denom", "description",
                     "memo", "id", "category")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("gc_detect_csv_format identifies paypal", {
  header <- "Date,Time,TimeZone,Name,Type,Status,Currency,Gross,Fee,Net,From Email Address,To Email Address,Transaction ID"
  result <- gc_detect_csv_format(header)
  expect_equal(result, "paypal")
})

test_that("gc_detect_csv_format identifies stripe", {
  header <- "id,Created (UTC),Amount,Status,Description"
  result <- gc_detect_csv_format(header)
  expect_equal(result, "stripe")
})

test_that("gc_detect_csv_format returns empty for unknown", {
  header <- "col1,col2,col3"
  result <- gc_detect_csv_format(header)
  expect_equal(result, "")
})

test_that("gc_csv_format_info returns correct generic format", {
  info <- gc_csv_format_info("generic")
  expect_equal(info$name, "generic")
  expect_equal(info$date_col, 0)
  expect_equal(info$amount_col, 1)
  expect_equal(info$desc_col, 2)
  expect_equal(info$date_format, "YYYY-MM-DD")
})

test_that("gc_parse_csv handles empty content gracefully", {
  csv_content <- "date,amount,description\n"
  result <- gc_parse_csv(csv_content, "generic")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("gc_parse_csv skips zero-amount rows with no description", {
  csv_content <- "date,amount,description\n2024-01-15,0.00,\n2024-01-16,10.00,Real\n"
  result <- gc_parse_csv(csv_content, "generic")
  expect_equal(nrow(result), 1)
  expect_equal(result$description[1], "Real")
})
