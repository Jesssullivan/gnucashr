# test-price.R - Tests for Price and PriceDB R6 classes

test_that("Price can be created", {
  skip_if_not(inherits(Price, "R6ClassGenerator"),
              message = "Price class not available")

  price <- Price$new(
    commodity_guid = "abc123",
    currency_guid = "def456",
    value_num = 15025L,
    value_denom = 100L
  )

  expect_s3_class(price, "Price")
  expect_equal(price$value(), 150.25)
})

test_that("new_price helper creates Price from decimal", {
  skip_if_not(exists("new_price"),
              message = "new_price function not available")

  price <- new_price("abc", "def", 150.25)

  expect_s3_class(price, "Price")
  expect_equal(price$value(), 150.25, tolerance = 0.001)
})

test_that("Price set_value updates value", {
  skip_if_not(inherits(Price, "R6ClassGenerator"),
              message = "Price class not available")

  price <- Price$new(
    commodity_guid = "abc",
    currency_guid = "def",
    value_num = 10000L,
    value_denom = 100L
  )

  expect_equal(price$value(), 100)

  price$set_value(150.50)
  expect_equal(price$value(), 150.50, tolerance = 0.01)
})

test_that("Price convert_to_currency and convert_to_commodity work", {
  skip_if_not(inherits(Price, "R6ClassGenerator"),
              message = "Price class not available")

  # 1 AAPL = $150
  price <- Price$new(
    commodity_guid = "aapl",
    currency_guid = "usd",
    value_num = 15000L,
    value_denom = 100L
  )

  # 10 shares at $150 = $1500
  expect_equal(price$convert_to_currency(10), 1500)

  # $1500 / $150 = 10 shares
  expect_equal(price$convert_to_commodity(1500), 10)
})

test_that("Price as_tibble returns correct structure", {
  skip_if_not(inherits(Price, "R6ClassGenerator"),
              message = "Price class not available")

  price <- new_price("abc", "def", 100.00)
  df <- price$as_tibble()

  expect_s3_class(df, "tbl_df")
  expect_true(all(c("guid", "commodity_guid", "currency_guid",
                    "date", "value") %in% names(df)))
  expect_equal(nrow(df), 1)
})

test_that("PriceDB can be created empty", {
  skip_if_not(inherits(PriceDB, "R6ClassGenerator"),
              message = "PriceDB class not available")

  pdb <- PriceDB$new()

  expect_s3_class(pdb, "PriceDB")
  expect_equal(pdb$count(), 0)
})

test_that("PriceDB add and get_price work", {
  skip_if_not(inherits(PriceDB, "R6ClassGenerator"),
              message = "PriceDB class not available")

  pdb <- PriceDB$new()

  price <- new_price("aapl", "usd", 150.00,
                     date = as.POSIXct("2024-01-15"))
  pdb$add(price)

  expect_equal(pdb$count(), 1)

  retrieved <- pdb$get_price("aapl", "usd")
  expect_equal(retrieved$value(), 150.00)
})

test_that("PriceDB get_prices returns ordered by date", {
  skip_if_not(inherits(PriceDB, "R6ClassGenerator"),
              message = "PriceDB class not available")

  pdb <- PriceDB$new()

  # Add older price first
  pdb$add(new_price("aapl", "usd", 140.00,
                    date = as.POSIXct("2024-01-01")))
  # Add newer price
  pdb$add(new_price("aapl", "usd", 150.00,
                    date = as.POSIXct("2024-01-15")))

  prices <- pdb$get_prices("aapl", "usd")

  # Most recent should be first
  expect_equal(prices[[1]]$value(), 150.00)
})

test_that("PriceDB get_rate finds direct and inverse rates", {
  skip_if_not(inherits(PriceDB, "R6ClassGenerator"),
              message = "PriceDB class not available")

  pdb <- PriceDB$new()

  # 1 EUR = 1.10 USD
  pdb$add(new_price("eur", "usd", 1.10))

  # Direct rate
  rate <- pdb$get_rate("eur", "usd")
  expect_equal(rate, 1.10)

  # Inverse rate
  inverse_rate <- pdb$get_rate("usd", "eur")
  expect_equal(inverse_rate, 1/1.10, tolerance = 0.001)
})

test_that("PriceDB convert works", {
  skip_if_not(inherits(PriceDB, "R6ClassGenerator"),
              message = "PriceDB class not available")

  pdb <- PriceDB$new()
  pdb$add(new_price("eur", "usd", 1.10))

  # 100 EUR to USD
  result <- pdb$convert(100, "eur", "usd")
  expect_equal(result, 110)

  # Same currency returns same amount
  result <- pdb$convert(100, "usd", "usd")
  expect_equal(result, 100)
})

test_that("pricedb_from_df creates PriceDB from data frame", {
  skip_if_not(exists("pricedb_from_df"),
              message = "pricedb_from_df function not available")

  df <- tibble::tibble(
    guid = c(generate_guid(), generate_guid()),
    commodity_guid = c("aapl", "googl"),
    currency_guid = c("usd", "usd"),
    date = c(as.POSIXct("2024-01-15"), as.POSIXct("2024-01-15")),
    source = c("user:price", "user:price"),
    type = c("last", "last"),
    value_num = c(15000L, 14000L),
    value_denom = c(100L, 100L)
  )

  pdb <- pricedb_from_df(df)

  expect_s3_class(pdb, "PriceDB")
  expect_equal(pdb$count(), 2)
})
