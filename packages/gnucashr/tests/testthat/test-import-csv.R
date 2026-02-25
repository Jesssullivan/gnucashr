# Test CSV Import Functions

# =============================================================================
# Test Data Creation Helpers
# =============================================================================

#' Create a temporary PayPal CSV file for testing
#'
#' @param data Optional tibble with custom data
#' @return Path to temporary file
create_temp_paypal_csv <- function(data = NULL) {
  if (is.null(data)) {
    data <- tibble::tibble(
      Date = c("01/15/2024", "01/16/2024", "01/17/2024"),
      Time = c("10:30:00", "14:45:00", "09:15:00"),
      TimeZone = c("PST", "PST", "PST"),
      Name = c("Customer One", "Customer Two", "Refund - Customer One"),
      Type = c("Payment", "Payment", "Refund"),
      Status = c("Completed", "Completed", "Completed"),
      Currency = c("USD", "USD", "USD"),
      Gross = c("100.00", "50.00", "-25.00"),
      Fee = c("-2.90", "-1.45", "0.73"),
      Net = c("97.10", "48.55", "-24.27"),
      `Transaction ID` = c("TXN001", "TXN002", "TXN003")
    )
  }

  path <- tempfile(fileext = ".csv")
  readr::write_csv(data, path)
  path
}


#' Create a temporary Stripe CSV file for testing
#'
#' @param data Optional tibble with custom data
#' @return Path to temporary file
create_temp_stripe_csv <- function(data = NULL) {
  if (is.null(data)) {
    data <- tibble::tibble(
      id = c("txn_001", "txn_002", "txn_003"),
      `Created (UTC)` = c("2024-01-15 18:30:00", "2024-01-16 22:45:00", "2024-01-17 17:15:00"),
      Amount = c("10000", "5000", "-2500"),  # in cents
      Fee = c("290", "145", "0"),
      Net = c("9710", "4855", "-2500"),
      Currency = c("usd", "usd", "usd"),
      Description = c("Payment from customer@example.com", "Payment from test@test.com", "Refund"),
      Type = c("charge", "charge", "refund")
    )
  }

  path <- tempfile(fileext = ".csv")
  readr::write_csv(data, path)
  path
}


#' Create a temporary QuickBooks CSV file for testing
#'
#' @param data Optional tibble with custom data
#' @param format "qbo" or "desktop"
#' @return Path to temporary file
create_temp_quickbooks_csv <- function(data = NULL, format = "qbo") {
  if (is.null(data)) {
    if (format == "qbo") {
      data <- tibble::tibble(
        Date = c("01/15/2024", "01/16/2024", "01/17/2024"),
        `Transaction Type` = c("Invoice", "Payment", "Expense"),
        Num = c("1001", "1002", "1003"),
        Name = c("Client ABC", "Client XYZ", "Office Supplies"),
        Memo = c("Consulting services", "Payment received", "Paper and pens"),
        Account = c("Accounts Receivable", "Checking", "Office Expenses"),
        Amount = c("1500.00", "750.00", "-125.50")
      )
    } else {
      data <- tibble::tibble(
        Date = c("01/15/2024", "01/16/2024", "01/17/2024"),
        Type = c("Invoice", "Payment", "Check"),
        Num = c("1001", "1002", "1003"),
        Name = c("Client ABC", "Client XYZ", "Office Depot"),
        Memo = c("Services", "Payment", "Supplies"),
        Account = c("AR", "Checking", "Checking"),
        Debit = c("", "", "125.50"),
        Credit = c("1500.00", "750.00", "")
      )
    }
  }

  path <- tempfile(fileext = ".csv")
  readr::write_csv(data, path)
  path
}


# =============================================================================
# PayPal Import Tests
# =============================================================================

test_that("import_paypal_csv loads valid PayPal file", {
  path <- create_temp_paypal_csv()
  withr::defer(unlink(path))

  result <- import_paypal_csv(path)

  expect_s3_class(result, "gnucashr_csv_import")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_true(is_csv_import(result))

  # Check standard columns exist
  expect_true(all(c("date", "description", "amount", "currency",
                    "external_id", "account", "memo", "category") %in% names(result)))

  # Check values
  expect_equal(result$account[1], "PayPal")
  expect_equal(result$external_id[1], "TXN001")
  expect_equal(result$category[1], "payment")
  expect_equal(result$category[3], "refund")
})


test_that("import_paypal_csv handles missing file", {
  expect_error(
    import_paypal_csv("nonexistent_file.csv"),
    "not found"
  )
})


test_that("import_paypal_csv handles empty file", {
  path <- tempfile(fileext = ".csv")
  withr::defer(unlink(path))

  writeLines("Date,Time,Name,Type,Status,Currency,Gross,Fee,Net,Transaction ID", path)

  expect_error(
    import_paypal_csv(path),
    "empty"
  )
})


test_that("import_paypal_csv filters pending transactions by default", {
  data <- tibble::tibble(
    Date = c("01/15/2024", "01/16/2024"),
    Time = c("10:30:00", "14:45:00"),
    TimeZone = c("PST", "PST"),
    Name = c("Customer One", "Customer Two"),
    Type = c("Payment", "Payment"),
    Status = c("Completed", "Pending"),
    Currency = c("USD", "USD"),
    Gross = c("100.00", "50.00"),
    Fee = c("-2.90", "-1.45"),
    Net = c("97.10", "48.55"),
    `Transaction ID` = c("TXN001", "TXN002")
  )

  path <- create_temp_paypal_csv(data)
  withr::defer(unlink(path))

  result <- import_paypal_csv(path)
  expect_equal(nrow(result), 1)

  result_with_pending <- import_paypal_csv(path, include_pending = TRUE)
  expect_equal(nrow(result_with_pending), 2)
})


test_that("import_paypal_csv parses amounts correctly", {
  data <- tibble::tibble(
    Date = c("01/15/2024", "01/16/2024"),
    Time = c("10:30:00", "14:45:00"),
    TimeZone = c("PST", "PST"),
    Name = c("Customer One", "Refund"),
    Type = c("Payment", "Refund"),
    Status = c("Completed", "Completed"),
    Currency = c("USD", "EUR"),
    Gross = c("1,234.56", "-500.00"),
    Fee = c("-35.80", "14.50"),
    Net = c("1,198.76", "-485.50"),
    `Transaction ID` = c("TXN001", "TXN002")
  )

  path <- create_temp_paypal_csv(data)
  withr::defer(unlink(path))

  result <- import_paypal_csv(path)

  expect_equal(result$amount[1], 1234.56)
  expect_equal(result$amount[2], -500.00)
  expect_equal(result$currency[1], "USD")
  expect_equal(result$currency[2], "EUR")
})


# =============================================================================
# Stripe Import Tests
# =============================================================================

test_that("import_stripe_csv loads valid Stripe file", {
  path <- create_temp_stripe_csv()
  withr::defer(unlink(path))

  result <- import_stripe_csv(path)

  expect_s3_class(result, "gnucashr_csv_import")
  expect_equal(nrow(result), 3)

  # Check standard columns
  expect_true(all(c("date", "description", "amount", "currency",
                    "external_id", "account", "memo", "category") %in% names(result)))

  # Check values
  expect_equal(result$account[1], "Stripe")
  expect_equal(result$external_id[1], "txn_001")
  expect_equal(result$category[1], "charge")
  expect_equal(result$category[3], "refund")
})


test_that("import_stripe_csv handles missing file", {
  expect_error(
    import_stripe_csv("nonexistent_stripe.csv"),
    "not found"
  )
})


test_that("import_stripe_csv converts cents to dollars", {
  data <- tibble::tibble(
    id = c("txn_001", "txn_002"),
    `Created (UTC)` = c("2024-01-15 18:30:00", "2024-01-16 22:45:00"),
    Amount = c("15000", "7500"),  # 150.00 and 75.00 in cents
    Fee = c("435", "218"),
    Net = c("14565", "7282"),
    Currency = c("usd", "usd"),
    Description = c("Test charge 1", "Test charge 2"),
    Type = c("charge", "charge")
  )

  path <- create_temp_stripe_csv(data)
  withr::defer(unlink(path))

  result <- import_stripe_csv(path)

  # Should be converted from cents
  expect_equal(result$amount[1], 150.00)
  expect_equal(result$amount[2], 75.00)
})


test_that("import_stripe_csv handles different date formats", {
  data <- tibble::tibble(
    id = c("txn_001"),
    `Created (UTC)` = c("2024-01-15T18:30:00Z"),
    Amount = c("100.00"),
    Fee = c("2.90"),
    Net = c("97.10"),
    Currency = c("usd"),
    Description = c("Test"),
    Type = c("charge")
  )

  path <- create_temp_stripe_csv(data)
  withr::defer(unlink(path))

  result <- import_stripe_csv(path)
  expect_equal(as.character(result$date[1]), "2024-01-15")
})


# =============================================================================
# QuickBooks Import Tests
# =============================================================================

test_that("import_quickbooks_csv loads QBO format", {
  path <- create_temp_quickbooks_csv(format = "qbo")
  withr::defer(unlink(path))

  result <- import_quickbooks_csv(path)

  expect_s3_class(result, "gnucashr_csv_import")
  expect_equal(nrow(result), 3)

  # Check standard columns
  expect_true(all(c("date", "description", "amount", "currency",
                    "external_id", "account", "memo", "category") %in% names(result)))

  # Check values
  expect_equal(result$category[1], "invoice")
  expect_equal(result$category[2], "payment")
  expect_equal(result$category[3], "expense")
})


test_that("import_quickbooks_csv handles desktop format with debit/credit", {
  path <- create_temp_quickbooks_csv(format = "desktop")
  withr::defer(unlink(path))

  result <- import_quickbooks_csv(path, format = "desktop")

  expect_s3_class(result, "gnucashr_csv_import")
  expect_equal(nrow(result), 3)

  # Credit should be positive, debit negative
  expect_equal(result$amount[1], 1500.00)  # Credit
  expect_equal(result$amount[2], 750.00)   # Credit
  expect_equal(result$amount[3], -125.50)  # Debit
})


test_that("import_quickbooks_csv auto-detects format", {
  # QBO format
  path_qbo <- create_temp_quickbooks_csv(format = "qbo")
  withr::defer(unlink(path_qbo))

  result_qbo <- import_quickbooks_csv(path_qbo, format = "auto")
  expect_s3_class(result_qbo, "gnucashr_csv_import")

  # Desktop format
  path_desktop <- create_temp_quickbooks_csv(format = "desktop")
  withr::defer(unlink(path_desktop))

  result_desktop <- import_quickbooks_csv(path_desktop, format = "auto")
  expect_s3_class(result_desktop, "gnucashr_csv_import")
})


test_that("import_quickbooks_csv handles missing file", {
  expect_error(
    import_quickbooks_csv("nonexistent_qb.csv"),
    "not found"
  )
})


# =============================================================================
# Combine CSV Imports Tests
# =============================================================================

test_that("combine_csv_imports merges multiple imports", {
  paypal_path <- create_temp_paypal_csv()
  stripe_path <- create_temp_stripe_csv()
  withr::defer({
    unlink(paypal_path)
    unlink(stripe_path)
  })

  paypal <- import_paypal_csv(paypal_path)
  stripe <- import_stripe_csv(stripe_path)

  combined <- combine_csv_imports(paypal, stripe)

  expect_s3_class(combined, "gnucashr_csv_import")
  expect_equal(nrow(combined), nrow(paypal) + nrow(stripe))

  # Should have source column
  expect_true("source" %in% names(combined) || "account" %in% names(combined))

  # Metadata should indicate combined
  expect_equal(attr(combined, "source_type"), "combined")
})


test_that("combine_csv_imports handles deduplication", {
  # Create two files with one duplicate
  data1 <- tibble::tibble(
    Date = c("01/15/2024", "01/16/2024"),
    Time = c("10:30:00", "14:45:00"),
    TimeZone = c("PST", "PST"),
    Name = c("Customer One", "Customer Two"),
    Type = c("Payment", "Payment"),
    Status = c("Completed", "Completed"),
    Currency = c("USD", "USD"),
    Gross = c("100.00", "50.00"),
    Fee = c("-2.90", "-1.45"),
    Net = c("97.10", "48.55"),
    `Transaction ID` = c("TXN001", "TXN002")
  )

  data2 <- tibble::tibble(
    Date = c("01/15/2024", "01/17/2024"),  # First row is duplicate
    Time = c("10:30:00", "09:00:00"),
    TimeZone = c("PST", "PST"),
    Name = c("Customer One", "Customer Three"),  # Same as data1 row 1
    Type = c("Payment", "Payment"),
    Status = c("Completed", "Completed"),
    Currency = c("USD", "USD"),
    Gross = c("100.00", "75.00"),
    Fee = c("-2.90", "-2.18"),
    Net = c("97.10", "72.82"),
    `Transaction ID` = c("TXN001_DUP", "TXN003")
  )

  path1 <- create_temp_paypal_csv(data1)
  path2 <- create_temp_paypal_csv(data2)
  withr::defer({
    unlink(path1)
    unlink(path2)
  })

  import1 <- import_paypal_csv(path1)
  import2 <- import_paypal_csv(path2)

  # Without dedup
  combined_no_dedup <- combine_csv_imports(import1, import2, deduplicate = FALSE)
  expect_equal(nrow(combined_no_dedup), 4)

  # With dedup
  combined_dedup <- combine_csv_imports(import1, import2, deduplicate = TRUE)
  expect_equal(nrow(combined_dedup), 3)  # One duplicate removed
})


test_that("combine_csv_imports fails for non-imports", {
  path <- create_temp_paypal_csv()
  withr::defer(unlink(path))

  paypal <- import_paypal_csv(path)
  regular_df <- data.frame(x = 1:3)

  expect_error(
    combine_csv_imports(paypal, regular_df),
    "not a gnucashr_csv_import"
  )
})


test_that("combine_csv_imports handles empty list", {
  expect_error(
    combine_csv_imports(),
    "No imports provided"
  )
})


# =============================================================================
# Validation Tests
# =============================================================================

test_that("validate_csv_import detects issues", {
  # Create data with issues
  data <- tibble::tibble(
    Date = c("01/15/2024", "", "01/17/2024", "01/15/2024"),
    Time = c("10:30:00", "14:45:00", "09:15:00", "10:30:00"),
    TimeZone = c("PST", "PST", "PST", "PST"),
    Name = c("Customer One", "Customer Two", "Customer Three", "Customer One"),  # Duplicate
    Type = c("Payment", "Payment", "Payment", "Payment"),
    Status = c("Completed", "Completed", "Completed", "Completed"),
    Currency = c("USD", "USD", "EUR", "USD"),  # Multiple currencies
    Gross = c("100.00", "0.00", "50.00", "100.00"),  # Zero amount
    Fee = c("-2.90", "0.00", "-1.45", "-2.90"),
    Net = c("97.10", "0.00", "48.55", "97.10"),
    `Transaction ID` = c("TXN001", "", "TXN003", "TXN001")  # Missing ID
  )

  path <- create_temp_paypal_csv(data)
  withr::defer(unlink(path))

  # This should import with filtering
  result <- import_paypal_csv(path)

  # Validation should find issues
  expect_warning(
    validation <- validate_csv_import(result),
    regexp = NULL  # Any warning
  )

  expect_false(validation$valid)
  expect_true(length(validation$issues) > 0)
})


test_that("validate_csv_import passes for clean data", {
  path <- create_temp_paypal_csv()
  withr::defer(unlink(path))

  result <- import_paypal_csv(path)

  # Should not warn for clean data
  validation <- validate_csv_import(result)
  expect_true(validation$valid)
  expect_equal(length(validation$issues), 0)
})


test_that("validate_csv_import strict mode throws error", {
  # Create data with issues
  data <- tibble::tibble(
    Date = c("01/15/2024", "01/15/2024"),
    Time = c("10:30:00", "10:30:00"),
    TimeZone = c("PST", "PST"),
    Name = c("Customer One", "Customer One"),  # Duplicate
    Type = c("Payment", "Payment"),
    Status = c("Completed", "Completed"),
    Currency = c("USD", "USD"),
    Gross = c("100.00", "100.00"),
    Fee = c("-2.90", "-2.90"),
    Net = c("97.10", "97.10"),
    `Transaction ID` = c("TXN001", "TXN002")
  )

  path <- create_temp_paypal_csv(data)
  withr::defer(unlink(path))

  result <- import_paypal_csv(path)

  expect_error(
    validate_csv_import(result, strict = TRUE),
    "validation"
  )
})


# =============================================================================
# Utility Function Tests
# =============================================================================

test_that("is_csv_import correctly identifies imports", {
  path <- create_temp_paypal_csv()
  withr::defer(unlink(path))

  result <- import_paypal_csv(path)

  expect_true(is_csv_import(result))
  expect_false(is_csv_import(data.frame(x = 1:3)))
  expect_false(is_csv_import(list(a = 1)))
  expect_false(is_csv_import(NULL))
})


test_that("print method works for csv imports", {
  path <- create_temp_paypal_csv()
  withr::defer(unlink(path))

  result <- import_paypal_csv(path)

  # Should not error
  expect_output(print(result), "gnucashr_csv_import")
  expect_output(print(result), "Source:")
  expect_output(print(result), "Rows:")
})


test_that("preview_csv_mapping provides helpful output", {
  path <- create_temp_paypal_csv()
  withr::defer(unlink(path))

  # Should produce output without error
  expect_output(
    preview_csv_mapping(path, import_paypal_csv),
    "CSV Preview"
  )
})


# =============================================================================
# Edge Cases and Error Handling
# =============================================================================

test_that("imports handle malformed CSV gracefully", {
  # Create a CSV with inconsistent columns
  path <- tempfile(fileext = ".csv")
  withr::defer(unlink(path))

  writeLines(c(
    "Date,Name,Amount",
    "01/15/2024,Test,100",
    "01/16/2024,Test2"  # Missing column
  ), path)

  # Should error with helpful message about unrecognized format
  expect_error(import_paypal_csv(path), "Unrecognized")
})


test_that("imports handle different date formats", {
  # ISO format dates
  data <- tibble::tibble(
    Date = c("2024-01-15", "2024-01-16", "2024-01-17"),
    Time = c("10:30:00", "14:45:00", "09:15:00"),
    TimeZone = c("PST", "PST", "PST"),
    Name = c("Customer One", "Customer Two", "Customer Three"),
    Type = c("Payment", "Payment", "Payment"),
    Status = c("Completed", "Completed", "Completed"),
    Currency = c("USD", "USD", "USD"),
    Gross = c("100.00", "50.00", "75.00"),
    Fee = c("-2.90", "-1.45", "-2.18"),
    Net = c("97.10", "48.55", "72.82"),
    `Transaction ID` = c("TXN001", "TXN002", "TXN003")
  )

  path <- create_temp_paypal_csv(data)
  withr::defer(unlink(path))

  result <- import_paypal_csv(path)

  expect_equal(as.character(result$date[1]), "2024-01-15")
  expect_equal(nrow(result), 3)
})


# =============================================================================
# Sample File Tests (if inst/extdata files exist)
# =============================================================================

test_that("sample PayPal CSV can be imported", {
  sample_path <- system.file("extdata", "sample-paypal.csv", package = "gnucashr")
  skip_if(sample_path == "", "Sample PayPal CSV not found")

  result <- import_paypal_csv(sample_path)

  expect_s3_class(result, "gnucashr_csv_import")
  expect_gt(nrow(result), 0)
})


test_that("sample Stripe CSV can be imported", {
  sample_path <- system.file("extdata", "sample-stripe.csv", package = "gnucashr")
  skip_if(sample_path == "", "Sample Stripe CSV not found")

  result <- import_stripe_csv(sample_path)

  expect_s3_class(result, "gnucashr_csv_import")
  expect_gt(nrow(result), 0)
})
