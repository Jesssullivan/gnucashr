# test-commodity.R - Tests for Commodity R6 class

test_that("Commodity can be created with minimal parameters", {
  skip_if_not(inherits(Commodity, "R6ClassGenerator"),
              message = "Commodity class not available")

  cmdty <- Commodity$new(mnemonic = "USD")

  expect_s3_class(cmdty, "Commodity")
  expect_equal(cmdty$mnemonic, "USD")
  expect_equal(cmdty$namespace, "CURRENCY")
  expect_equal(cmdty$fraction, 100L)
})

test_that("Commodity generates GUID if not provided", {
  skip_if_not(inherits(Commodity, "R6ClassGenerator"),
              message = "Commodity class not available")

  cmdty <- Commodity$new(mnemonic = "EUR")

  expect_true(nchar(cmdty$guid) == 32)
  expect_true(validate_guid(cmdty$guid))
})

test_that("currency() helper creates currency commodity", {
  skip_if_not(exists("currency"),
              message = "currency function not available")

  usd <- currency("USD")

  expect_true(usd$is_currency())
  expect_false(usd$is_security())
  expect_equal(usd$namespace, "CURRENCY")
  expect_equal(usd$mnemonic, "USD")
  expect_equal(usd$fraction, 100L)
})

test_that("JPY has no decimal places", {
  skip_if_not(exists("currency"),
              message = "currency function not available")

  jpy <- currency("JPY")

  expect_equal(jpy$fraction, 1L)
  expect_equal(jpy$decimal_places(), 0)
})

test_that("security() helper creates security commodity", {
  skip_if_not(exists("security"),
              message = "security function not available")

  aapl <- security("AAPL", "NASDAQ", "Apple Inc.")

  expect_false(aapl$is_currency())
  expect_true(aapl$is_security())
  expect_equal(aapl$namespace, "NASDAQ")
  expect_equal(aapl$mnemonic, "AAPL")
  expect_equal(aapl$fullname, "Apple Inc.")
})

test_that("Commodity identifier() works", {
  skip_if_not(inherits(Commodity, "R6ClassGenerator"),
              message = "Commodity class not available")

  cmdty <- Commodity$new(namespace = "NYSE", mnemonic = "IBM")

  expect_equal(cmdty$identifier(), "NYSE:IBM")
})

test_that("Commodity decimal_places() calculates correctly", {
  skip_if_not(inherits(Commodity, "R6ClassGenerator"),
              message = "Commodity class not available")

  # Cents (2 decimal places)
  usd <- Commodity$new(mnemonic = "USD", fraction = 100L)
  expect_equal(usd$decimal_places(), 2)

  # 4 decimal places for stocks
  stock <- Commodity$new(namespace = "NYSE", mnemonic = "TEST", fraction = 10000L)
  expect_equal(stock$decimal_places(), 4)
})

test_that("Commodity to_smallest_unit and from_smallest_unit work", {
  skip_if_not(inherits(Commodity, "R6ClassGenerator"),
              message = "Commodity class not available")

  usd <- Commodity$new(mnemonic = "USD", fraction = 100L)

  # $10.50 = 1050 cents
  expect_equal(usd$to_smallest_unit(10.50), 1050L)
  expect_equal(usd$from_smallest_unit(1050L), 10.50)
})

test_that("Commodity format_amount works", {
  skip_if_not(inherits(Commodity, "R6ClassGenerator"),
              message = "Commodity class not available")

  usd <- Commodity$new(namespace = "CURRENCY", mnemonic = "USD", fraction = 100L)

  formatted <- usd$format_amount(1234.56)
  expect_true(grepl("1,234.56", formatted))
  expect_true(grepl("\\$", formatted))
})

test_that("Commodity as_tibble returns correct structure", {
  skip_if_not(inherits(Commodity, "R6ClassGenerator"),
              message = "Commodity class not available")

  cmdty <- Commodity$new(
    mnemonic = "USD",
    fullname = "US Dollar"
  )

  df <- cmdty$as_tibble()

  expect_s3_class(df, "tbl_df")
  expect_true("guid" %in% names(df))
  expect_true("mnemonic" %in% names(df))
  expect_true("fullname" %in% names(df))
  expect_equal(nrow(df), 1)
})

test_that("commodities_from_df creates list of Commodity objects", {
  skip_if_not(exists("commodities_from_df"),
              message = "commodities_from_df function not available")

  df <- tibble::tibble(
    guid = c(generate_guid(), generate_guid()),
    namespace = c("CURRENCY", "NASDAQ"),
    mnemonic = c("USD", "AAPL"),
    fullname = c("US Dollar", "Apple Inc."),
    cusip = c(NA, "037833100"),
    fraction = c(100L, 10000L)
  )

  commodities <- commodities_from_df(df)

  expect_type(commodities, "list")
  expect_equal(length(commodities), 2)
  expect_true("CURRENCY:USD" %in% names(commodities))
  expect_true("NASDAQ:AAPL" %in% names(commodities))
})
