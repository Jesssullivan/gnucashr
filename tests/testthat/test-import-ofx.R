# Tests for OFX/QFX import functionality

# Sample OFX 1.x content for testing (SGML format)
sample_ofx_v1 <- '
OFXHEADER:100
DATA:OFXSGML
VERSION:102
SECURITY:NONE
ENCODING:USASCII
CHARSET:1252
COMPRESSION:NONE
OLDFILEUID:NONE
NEWFILEUID:NONE

<OFX>
<SIGNONMSGSRSV1>
<SONRS>
<STATUS>
<CODE>0
<SEVERITY>INFO
</STATUS>
<DTSERVER>20240115120000
<LANGUAGE>ENG
<FI>
<ORG>Test Bank
<FID>12345
</FI>
</SONRS>
</SIGNONMSGSRSV1>
<BANKMSGSRSV1>
<STMTTRNRS>
<TRNUID>1001
<STATUS>
<CODE>0
<SEVERITY>INFO
</STATUS>
<STMTRS>
<CURDEF>USD
<BANKACCTFROM>
<BANKID>123456789
<ACCTID>9876543210
<ACCTTYPE>CHECKING
</BANKACCTFROM>
<BANKTRANLIST>
<DTSTART>20240101
<DTEND>20240115
<STMTTRN>
<TRNTYPE>DEBIT
<DTPOSTED>20240105120000
<TRNAMT>-42.50
<FITID>2024010500001
<NAME>GROCERY STORE
<MEMO>Purchase groceries
</STMTTRN>
<STMTTRN>
<TRNTYPE>CREDIT
<DTPOSTED>20240110
<TRNAMT>1500.00
<FITID>2024011000001
<NAME>DIRECT DEPOSIT
<MEMO>Payroll
</STMTTRN>
<STMTTRN>
<TRNTYPE>CHECK
<DTPOSTED>20240112
<TRNAMT>-200.00
<FITID>2024011200001
<NAME>Check 1234
</STMTTRN>
</BANKTRANLIST>
<LEDGERBAL>
<BALAMT>5432.10
<DTASOF>20240115
</LEDGERBAL>
</STMTRS>
</STMTTRNRS>
</BANKMSGSRSV1>
</OFX>
'

# Sample OFX 2.x content (XML format)
sample_ofx_v2 <- '<?xml version="1.0" encoding="UTF-8"?>
<?OFX OFXHEADER="200" VERSION="220" SECURITY="NONE" OLDFILEUID="NONE" NEWFILEUID="NONE"?>
<OFX>
<SIGNONMSGSRSV1>
<SONRS>
<STATUS><CODE>0</CODE><SEVERITY>INFO</SEVERITY></STATUS>
<DTSERVER>20240115120000</DTSERVER>
<LANGUAGE>ENG</LANGUAGE>
<FI><ORG>XML Test Bank</ORG><FID>67890</FID></FI>
</SONRS>
</SIGNONMSGSRSV1>
<BANKMSGSRSV1>
<STMTTRNRS>
<TRNUID>2001</TRNUID>
<STATUS><CODE>0</CODE><SEVERITY>INFO</SEVERITY></STATUS>
<STMTRS>
<CURDEF>EUR</CURDEF>
<BANKACCTFROM>
<BANKID>987654321</BANKID>
<ACCTID>1234567890</ACCTID>
<ACCTTYPE>SAVINGS</ACCTTYPE>
</BANKACCTFROM>
<BANKTRANLIST>
<DTSTART>20240101</DTSTART>
<DTEND>20240115</DTEND>
<STMTTRN>
<TRNTYPE>DEBIT</TRNTYPE>
<DTPOSTED>20240108</DTPOSTED>
<TRNAMT>-75.00</TRNAMT>
<FITID>XML2024010800001</FITID>
<NAME>UTILITY PAYMENT</NAME>
<MEMO>Electric bill</MEMO>
</STMTTRN>
<STMTTRN>
<TRNTYPE>CREDIT</TRNTYPE>
<DTPOSTED>20240115</DTPOSTED>
<TRNAMT>250.00</TRNAMT>
<FITID>XML2024011500001</FITID>
<NAME>INTEREST PAYMENT</NAME>
</STMTTRN>
</BANKTRANLIST>
</STMTRS>
</STMTTRNRS>
</BANKMSGSRSV1>
</OFX>
'


# Helper to create temp OFX file
create_temp_ofx <- function(content, suffix = ".ofx") {
  f <- tempfile(fileext = suffix)
  writeLines(content, f)
  f
}


test_that("detect_ofx_version_cpp correctly identifies OFX versions", {
  # OFX 1.x (SGML)
  v1_result <- detect_ofx_version_cpp(sample_ofx_v1)
  expect_equal(v1_result, "1")

  # OFX 2.x (XML)
  v2_result <- detect_ofx_version_cpp(sample_ofx_v2)
  expect_equal(v2_result, "2")

  # Unknown format
  unknown <- detect_ofx_version_cpp("This is not an OFX file")
  expect_equal(unknown, "unknown")
})


test_that("detect_ofx_version works with file paths", {
  skip_if_not(exists("detect_ofx_version"))

  # Create temp files
  f1 <- create_temp_ofx(sample_ofx_v1)
  f2 <- create_temp_ofx(sample_ofx_v2)

  on.exit({
    unlink(f1)
    unlink(f2)
  })

  expect_equal(detect_ofx_version(f1), "1")
  expect_equal(detect_ofx_version(f2), "2")
})


test_that("parse_ofx_cpp extracts transactions from OFX 1.x", {
  result <- parse_ofx_cpp(sample_ofx_v1)

  expect_equal(result$n_transactions, 3L)
  expect_equal(result$currency, "USD")

  # Check dates
  expect_equal(result$dates[1], "2024-01-05")
  expect_equal(result$dates[2], "2024-01-10")
  expect_equal(result$dates[3], "2024-01-12")

  # Check amounts
  expect_equal(result$amounts[1], -42.50)
  expect_equal(result$amounts[2], 1500.00)
  expect_equal(result$amounts[3], -200.00)

  # Check names
  expect_equal(result$names[1], "GROCERY STORE")
  expect_equal(result$names[2], "DIRECT DEPOSIT")
  expect_equal(result$names[3], "Check 1234")

  # Check FITIDs
  expect_equal(result$fitids[1], "2024010500001")
  expect_equal(result$fitids[2], "2024011000001")
  expect_equal(result$fitids[3], "2024011200001")

  # Check transaction types
  expect_equal(result$trntypes[1], "DEBIT")
  expect_equal(result$trntypes[2], "CREDIT")
  expect_equal(result$trntypes[3], "CHECK")
})


test_that("parse_ofx_cpp extracts transactions from OFX 2.x", {
  result <- parse_ofx_cpp(sample_ofx_v2)

  expect_equal(result$n_transactions, 2L)
  expect_equal(result$currency, "EUR")

  # Check first transaction
  expect_equal(result$dates[1], "2024-01-08")
  expect_equal(result$amounts[1], -75.00)
  expect_equal(result$names[1], "UTILITY PAYMENT")
  expect_equal(result$fitids[1], "XML2024010800001")

  # Check second transaction
  expect_equal(result$dates[2], "2024-01-15")
  expect_equal(result$amounts[2], 250.00)
  expect_equal(result$names[2], "INTEREST PAYMENT")
})


test_that("extract_ofx_account_info extracts account metadata", {
  result <- extract_ofx_account_info(sample_ofx_v1)

  expect_equal(result$bank_id, "123456789")
  expect_equal(result$account_id, "9876543210")
  expect_equal(result$account_type, "CHECKING")
  expect_equal(result$org_name, "Test Bank")
  expect_equal(result$fid, "12345")
  expect_equal(result$date_start, "2024-01-01")
  expect_equal(result$date_end, "2024-01-15")
})


test_that("import_ofx creates proper tibble structure", {
  skip_if_not(exists("import_ofx"))

  f <- create_temp_ofx(sample_ofx_v1)
  on.exit(unlink(f))

  result <- import_ofx(f)

  # Check tibble structure
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)

  # Check column names
  expected_cols <- c("date", "description", "amount", "currency",
                     "external_id", "memo", "transaction_type")
  expect_true(all(expected_cols %in% names(result)))

  # Check data types
  expect_s3_class(result$date, "Date")
  expect_type(result$description, "character")
  expect_type(result$amount, "double")
  expect_type(result$currency, "character")
  expect_type(result$external_id, "character")

  # Check attributes
  expect_equal(attr(result, "ofx_version"), "1")
  expect_type(attr(result, "ofx_account"), "list")
})


test_that("import_ofx handles empty files gracefully", {
  skip_if_not(exists("import_ofx"))

  # Create minimal OFX with no transactions
  empty_ofx <- '
OFXHEADER:100
DATA:OFXSGML
VERSION:102

<OFX>
<BANKMSGSRSV1>
<STMTTRNRS>
<STMTRS>
<CURDEF>USD
<BANKTRANLIST>
<DTSTART>20240101
<DTEND>20240115
</BANKTRANLIST>
</STMTRS>
</STMTTRNRS>
</BANKMSGSRSV1>
</OFX>
'

  f <- create_temp_ofx(empty_ofx)
  on.exit(unlink(f))

  expect_warning(result <- import_ofx(f), "No transactions found")
  expect_equal(nrow(result), 0)
  expect_true(all(c("date", "description", "amount") %in% names(result)))
})


test_that("import_ofx with account_mapping adds mapped_account column", {
  skip_if_not(exists("import_ofx"))

  f <- create_temp_ofx(sample_ofx_v1)
  on.exit(unlink(f))

  mapping <- list(
    DEBIT = "expense_guid_123",
    CREDIT = "income_guid_456"
  )

  result <- import_ofx(f, account_mapping = mapping)

  expect_true("mapped_account" %in% names(result))

  # DEBIT transaction should map to expense
  expect_equal(result$mapped_account[result$transaction_type == "DEBIT"],
               "expense_guid_123")

  # CREDIT transaction should map to income
  expect_equal(result$mapped_account[result$transaction_type == "CREDIT"],
               "income_guid_456")

  # CHECK transaction has no mapping
  expect_true(is.na(result$mapped_account[result$transaction_type == "CHECK"]))
})


test_that("import_ofx handles file not found", {
  skip_if_not(exists("import_ofx"))

  expect_error(
    import_ofx("/nonexistent/path/to/file.ofx"),
    "OFX file not found"
  )
})


test_that("parse_ofx_cpp handles malformed content gracefully", {
  # Missing required tags
  malformed <- "<OFX><STMTTRN><DTPOSTED>20240101</STMTTRN></OFX>"
  result <- parse_ofx_cpp(malformed)

  # Should parse without error but may have missing data

  expect_type(result, "list")
  expect_true("n_transactions" %in% names(result))
})


test_that("OFX date parsing handles various formats", {
  # Test via parse_ofx_cpp with different date formats
  ofx_short_date <- '
<OFX>
<STMTRS>
<CURDEF>USD
<BANKTRANLIST>
<STMTTRN>
<TRNTYPE>DEBIT
<DTPOSTED>20240315
<TRNAMT>-10.00
<FITID>TEST001
<NAME>Short Date Test
</STMTTRN>
</BANKTRANLIST>
</STMTRS>
</OFX>
'

  ofx_long_date <- '
<OFX>
<STMTRS>
<CURDEF>USD
<BANKTRANLIST>
<STMTTRN>
<TRNTYPE>DEBIT
<DTPOSTED>20240315143022.000[-5:EST]
<TRNAMT>-10.00
<FITID>TEST002
<NAME>Long Date Test
</STMTTRN>
</BANKTRANLIST>
</STMTRS>
</OFX>
'

  result1 <- parse_ofx_cpp(ofx_short_date)
  expect_equal(result1$dates[1], "2024-03-15")

  result2 <- parse_ofx_cpp(ofx_long_date)
  expect_equal(result2$dates[1], "2024-03-15")
})


test_that("print_ofx_summary produces output", {
  skip_if_not(exists("print_ofx_summary"))

  f <- create_temp_ofx(sample_ofx_v1)
  on.exit(unlink(f))

  ofx_data <- import_ofx(f)

  expect_output(print_ofx_summary(ofx_data), "OFX Import Summary")
  expect_output(print_ofx_summary(ofx_data), "Transactions:")
  expect_output(print_ofx_summary(ofx_data), "Date Range:")
})


test_that("get_ofx_account_info returns proper structure", {
  skip_if_not(exists("get_ofx_account_info"))

  f <- create_temp_ofx(sample_ofx_v1)
  on.exit(unlink(f))

  info <- get_ofx_account_info(f)

  expect_type(info, "list")
  expect_true("account_id" %in% names(info))
  expect_true("bank_id" %in% names(info))
  expect_true("account_type" %in% names(info))
  expect_true("org_name" %in% names(info))
})


# Integration test with sample.ofx file if it exists
test_that("import_ofx works with sample.ofx", {
  skip_if_not(exists("import_ofx"))

  sample_path <- system.file("extdata", "sample.ofx", package = "gnucashr")
  skip_if(sample_path == "", "sample.ofx not found")

  result <- import_ofx(sample_path)

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
})
