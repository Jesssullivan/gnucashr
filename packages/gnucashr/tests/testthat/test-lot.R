# test-lot.R - Tests for Lot and LotManager R6 classes

test_that("Lot can be created", {
  skip_if_not(inherits(Lot, "R6ClassGenerator"),
              message = "Lot class not available")

  lot <- Lot$new(account_guid = "abc123")

  expect_s3_class(lot, "Lot")
  expect_equal(lot$account_guid, "abc123")
  expect_false(lot$is_closed)
})

test_that("Lot add_split accumulates correctly", {
  skip_if_not(inherits(Lot, "R6ClassGenerator"),
              message = "Lot class not available")

  lot <- Lot$new(account_guid = "abc")

  # Buy 10 shares at $100 each
  lot$add_split("split1", quantity = 10, value = 1000,
                date = as.POSIXct("2024-01-01"))

  expect_equal(lot$quantity(), 10)
  expect_equal(lot$cost_basis(), 1000)
  expect_equal(lot$avg_cost(), 100)
})

test_that("Lot calculates realized gain", {
  skip_if_not(inherits(Lot, "R6ClassGenerator"),
              message = "Lot class not available")

  lot <- Lot$new(account_guid = "abc")

  # Buy 10 shares at $100
  lot$add_split("buy", quantity = 10, value = 1000,
                date = as.POSIXct("2024-01-01"))

  # Sell 10 shares at $150 (proceeds = 1500)
  lot$add_split("sell", quantity = -10, value = -1500,
                date = as.POSIXct("2024-06-01"))

  lot$is_closed <- TRUE

  # Realized gain = 1500 - 1000 = 500
  expect_equal(lot$realized_gain(), 500)
})

test_that("Lot unrealized_gain calculates correctly", {
  skip_if_not(inherits(Lot, "R6ClassGenerator"),
              message = "Lot class not available")

  lot <- Lot$new(account_guid = "abc")

  # Buy 10 shares at $100
  lot$add_split("buy", quantity = 10, value = 1000,
                date = as.POSIXct("2024-01-01"))

  # Current price $150, unrealized = (150 * 10) - 1000 = 500
  expect_equal(lot$unrealized_gain(150), 500)
})

test_that("Lot holding_period calculates days", {
  skip_if_not(inherits(Lot, "R6ClassGenerator"),
              message = "Lot class not available")

  lot <- Lot$new(account_guid = "abc")

  # Open lot 30 days ago
  open_date <- Sys.time() - (30 * 24 * 60 * 60)
  lot$add_split("buy", quantity = 10, value = 1000, date = open_date)

  hp <- lot$holding_period()
  expect_true(hp >= 29 && hp <= 31)  # Allow for timing variance
})

test_that("Lot is_long_term works", {
  skip_if_not(inherits(Lot, "R6ClassGenerator"),
              message = "Lot class not available")

  lot <- Lot$new(account_guid = "abc")

  # Open lot 400 days ago (> 1 year)
  open_date <- Sys.time() - (400 * 24 * 60 * 60)
  lot$add_split("buy", quantity = 10, value = 1000, date = open_date)

  expect_true(lot$is_long_term())
})

test_that("Lot as_tibble returns correct structure", {
  skip_if_not(inherits(Lot, "R6ClassGenerator"),
              message = "Lot class not available")

  lot <- Lot$new(account_guid = "abc", title = "Test Lot")
  lot$add_split("buy", quantity = 10, value = 1000,
                date = as.POSIXct("2024-01-01"))

  df <- lot$as_tibble()

  expect_s3_class(df, "tbl_df")
  expect_true(all(c("guid", "account_guid", "quantity",
                    "cost_basis", "avg_cost") %in% names(df)))
})

test_that("LotManager can be created", {
  skip_if_not(inherits(LotManager, "R6ClassGenerator"),
              message = "LotManager class not available")

  lm <- LotManager$new(account_guid = "abc", method = "FIFO")

  expect_s3_class(lm, "LotManager")
  expect_equal(lm$method, "FIFO")
})

test_that("LotManager totals across lots", {
  skip_if_not(inherits(LotManager, "R6ClassGenerator"),
              message = "LotManager class not available")

  lm <- LotManager$new(account_guid = "abc")

  lot1 <- Lot$new(account_guid = "abc")
  lot1$add_split("buy1", quantity = 10, value = 1000,
                 date = as.POSIXct("2024-01-01"))

  lot2 <- Lot$new(account_guid = "abc")
  lot2$add_split("buy2", quantity = 5, value = 600,
                 date = as.POSIXct("2024-02-01"))

  lm$add_lot(lot1)
  lm$add_lot(lot2)

  expect_equal(lm$total_quantity(), 15)
  expect_equal(lm$total_cost_basis(), 1600)
})

test_that("LotManager FIFO ordering works", {
  skip_if_not(inherits(LotManager, "R6ClassGenerator"),
              message = "LotManager class not available")

  lm <- LotManager$new(account_guid = "abc", method = "FIFO")

  # Add older lot first
  lot1 <- Lot$new(account_guid = "abc")
  lot1$add_split("buy1", quantity = 10, value = 1000,
                 date = as.POSIXct("2024-01-01"))

  # Add newer lot
  lot2 <- Lot$new(account_guid = "abc")
  lot2$add_split("buy2", quantity = 5, value = 600,
                 date = as.POSIXct("2024-02-01"))

  lm$add_lot(lot1)
  lm$add_lot(lot2)

  ordered <- lm$open_lots_ordered()

  # FIFO: oldest first
  dates <- purrr::map_dbl(ordered, ~ as.numeric(.x$open_date()))
  expect_true(dates[1] < dates[2])
})

test_that("LotManager LIFO ordering works", {
  skip_if_not(inherits(LotManager, "R6ClassGenerator"),
              message = "LotManager class not available")

  lm <- LotManager$new(account_guid = "abc", method = "LIFO")

  lot1 <- Lot$new(account_guid = "abc")
  lot1$add_split("buy1", quantity = 10, value = 1000,
                 date = as.POSIXct("2024-01-01"))

  lot2 <- Lot$new(account_guid = "abc")
  lot2$add_split("buy2", quantity = 5, value = 600,
                 date = as.POSIXct("2024-02-01"))

  lm$add_lot(lot1)
  lm$add_lot(lot2)

  ordered <- lm$open_lots_ordered()

  # LIFO: newest first
  dates <- purrr::map_dbl(ordered, ~ as.numeric(.x$open_date()))
  expect_true(dates[1] > dates[2])
})

test_that("LotManager select_lots_for_sale works", {
  skip_if_not(inherits(LotManager, "R6ClassGenerator"),
              message = "LotManager class not available")

  lm <- LotManager$new(account_guid = "abc", method = "FIFO")

  lot1 <- Lot$new(account_guid = "abc")
  lot1$add_split("buy1", quantity = 10, value = 1000,
                 date = as.POSIXct("2024-01-01"))

  lot2 <- Lot$new(account_guid = "abc")
  lot2$add_split("buy2", quantity = 5, value = 600,
                 date = as.POSIXct("2024-02-01"))

  lm$add_lot(lot1)
  lm$add_lot(lot2)

  # Sell 12 shares (should use all of lot1 and part of lot2)
  selected <- lm$select_lots_for_sale(12)

  expect_equal(length(selected), 2)
  total_qty <- sum(purrr::map_dbl(selected, ~ .x$quantity))
  expect_equal(total_qty, 12)
})
