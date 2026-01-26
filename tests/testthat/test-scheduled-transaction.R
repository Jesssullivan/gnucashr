# test-scheduled-transaction.R - Tests for ScheduledTransaction R6 class

test_that("ScheduledTransaction can be created", {
  skip_if_not(inherits(ScheduledTransaction, "R6ClassGenerator"),
              message = "ScheduledTransaction class not available")

  sx <- ScheduledTransaction$new(
    name = "Monthly Rent",
    start_date = as.Date("2024-01-01")
  )

  expect_s3_class(sx, "ScheduledTransaction")
  expect_equal(sx$name, "Monthly Rent")
  expect_true(sx$enabled)
})

test_that("ScheduledTransaction set_recurrence works", {
  skip_if_not(inherits(ScheduledTransaction, "R6ClassGenerator"),
              message = "ScheduledTransaction class not available")

  sx <- ScheduledTransaction$new(
    name = "Test",
    start_date = as.Date("2024-01-01")
  )

  sx$set_recurrence("week", mult = 2L)

  expect_equal(sx$recurrence_period_type, "week")
  expect_equal(sx$recurrence_mult, 2L)
})

test_that("monthly_scheduled helper creates monthly transaction", {
  skip_if_not(exists("monthly_scheduled"),
              message = "monthly_scheduled function not available")

  sx <- monthly_scheduled(
    name = "Rent",
    amount = 2000,
    debit_account_guid = "expense_acc",
    credit_account_guid = "bank_acc",
    start_date = as.Date("2024-01-01")
  )

  expect_s3_class(sx, "ScheduledTransaction")
  expect_equal(sx$recurrence_period_type, "month")
  expect_equal(sx$recurrence_mult, 1L)
  expect_equal(sx$total_amount(), 2000, tolerance = 0.01)
})

test_that("weekly_scheduled helper creates weekly transaction", {
  skip_if_not(exists("weekly_scheduled"),
              message = "weekly_scheduled function not available")

  sx <- weekly_scheduled(
    name = "Grocery Shopping",
    amount = 200,
    debit_account_guid = "grocery_acc",
    credit_account_guid = "bank_acc",
    start_date = as.Date("2024-01-01")
  )

  expect_s3_class(sx, "ScheduledTransaction")
  expect_equal(sx$recurrence_period_type, "week")
})

test_that("annual_scheduled helper creates annual transaction", {
  skip_if_not(exists("annual_scheduled"),
              message = "annual_scheduled function not available")

  sx <- annual_scheduled(
    name = "Insurance Premium",
    amount = 1200,
    debit_account_guid = "insurance_acc",
    credit_account_guid = "bank_acc",
    start_date = as.Date("2024-01-01")
  )

  expect_s3_class(sx, "ScheduledTransaction")
  expect_equal(sx$recurrence_period_type, "year")
})

test_that("ScheduledTransaction add_template_split works", {
  skip_if_not(inherits(ScheduledTransaction, "R6ClassGenerator"),
              message = "ScheduledTransaction class not available")

  sx <- ScheduledTransaction$new(
    name = "Test",
    start_date = as.Date("2024-01-01")
  )

  sx$add_template_split("acc1", 10000L, 100L)
  sx$add_template_split("acc2", -10000L, 100L)

  splits <- sx$template_splits()
  expect_length(splits, 2)
})

test_that("ScheduledTransaction total_amount calculates correctly", {
  skip_if_not(inherits(ScheduledTransaction, "R6ClassGenerator"),
              message = "ScheduledTransaction class not available")

  sx <- ScheduledTransaction$new(
    name = "Test",
    start_date = as.Date("2024-01-01")
  )

  # Debit $100 to expense, credit $100 from bank
  sx$add_template_split("expense", 10000L, 100L)
  sx$add_template_split("bank", -10000L, 100L)

  expect_equal(sx$total_amount(), 100)
})

test_that("ScheduledTransaction recurrence_description works", {
  skip_if_not(inherits(ScheduledTransaction, "R6ClassGenerator"),
              message = "ScheduledTransaction class not available")

  sx <- ScheduledTransaction$new(
    name = "Test",
    start_date = as.Date("2024-01-01")
  )

  sx$set_recurrence("month", mult = 1L)
  expect_equal(sx$recurrence_description(), "Every month")

  sx$set_recurrence("week", mult = 2L)
  expect_equal(sx$recurrence_description(), "Every 2 weeks")
})

test_that("ScheduledTransaction next_occurrence calculates correctly", {
  skip_if_not(inherits(ScheduledTransaction, "R6ClassGenerator"),
              message = "ScheduledTransaction class not available")

  # Start on a known date
  sx <- ScheduledTransaction$new(
    name = "Monthly",
    start_date = as.Date("2024-01-15")
  )
  sx$set_recurrence("month", mult = 1L)

  next_occ <- sx$next_occurrence()

  # Should be approximately one month from start
  expect_s3_class(next_occ, "Date")
  expect_true(next_occ > as.Date("2024-01-15"))
})

test_that("ScheduledTransaction disabled returns NA for next_occurrence", {
  skip_if_not(inherits(ScheduledTransaction, "R6ClassGenerator"),
              message = "ScheduledTransaction class not available")

  sx <- ScheduledTransaction$new(
    name = "Test",
    start_date = as.Date("2024-01-01"),
    enabled = FALSE
  )

  expect_true(is.na(sx$next_occurrence()))
})

test_that("ScheduledTransaction is_due works", {
  skip_if_not(inherits(ScheduledTransaction, "R6ClassGenerator"),
              message = "ScheduledTransaction class not available")

  # Create transaction starting in the past
  sx <- ScheduledTransaction$new(
    name = "Past Due",
    start_date = as.Date("2020-01-01")
  )
  sx$set_recurrence("month", mult = 1L)

  expect_true(sx$is_due())
})

test_that("ScheduledTransaction as_tibble returns correct structure", {
  skip_if_not(inherits(ScheduledTransaction, "R6ClassGenerator"),
              message = "ScheduledTransaction class not available")

  sx <- monthly_scheduled(
    name = "Test",
    amount = 100,
    debit_account_guid = "acc1",
    credit_account_guid = "acc2",
    start_date = as.Date("2024-01-01")
  )

  df <- sx$as_tibble()

  expect_s3_class(df, "tbl_df")
  expect_true(all(c("guid", "name", "enabled", "start_date",
                    "recurrence_period_type", "amount") %in% names(df)))
})

test_that("ScheduledTransactionManager can be created", {
  skip_if_not(inherits(ScheduledTransactionManager, "R6ClassGenerator"),
              message = "ScheduledTransactionManager class not available")

  manager <- ScheduledTransactionManager$new()

  expect_s3_class(manager, "ScheduledTransactionManager")
})

test_that("ScheduledTransactionManager add and all work", {
  skip_if_not(inherits(ScheduledTransactionManager, "R6ClassGenerator"),
              message = "ScheduledTransactionManager class not available")

  manager <- ScheduledTransactionManager$new()

  sx1 <- monthly_scheduled("Rent", 2000, "acc1", "acc2",
                           start_date = as.Date("2024-01-01"))
  sx2 <- weekly_scheduled("Groceries", 200, "acc3", "acc2",
                          start_date = as.Date("2024-01-01"))

  manager$add(sx1)
  manager$add(sx2)

  all_sx <- manager$all()
  expect_length(all_sx, 2)
})

test_that("ScheduledTransactionManager due returns due transactions", {
  skip_if_not(inherits(ScheduledTransactionManager, "R6ClassGenerator"),
              message = "ScheduledTransactionManager class not available")

  manager <- ScheduledTransactionManager$new()

  # Past transaction (due)
  past <- monthly_scheduled("Past", 100, "acc1", "acc2",
                           start_date = as.Date("2020-01-01"))

  # Future transaction (not due)
  future <- monthly_scheduled("Future", 100, "acc1", "acc2",
                             start_date = Sys.Date() + 365)

  manager$add(past)
  manager$add(future)

  due <- manager$due()
  expect_length(due, 1)
})

test_that("ScheduledTransactionManager upcoming filters by days", {
  skip_if_not(inherits(ScheduledTransactionManager, "R6ClassGenerator"),
              message = "ScheduledTransactionManager class not available")

  manager <- ScheduledTransactionManager$new()

  # Use weekly transaction (guaranteed within 30 days)
  sx <- weekly_scheduled("Test", 100, "acc1", "acc2",
                        start_date = Sys.Date())
  manager$add(sx)

  upcoming <- manager$upcoming(days = 30)
  expect_length(upcoming, 1)
})

test_that("ScheduledTransactionManager as_tibble returns all transactions", {
  skip_if_not(inherits(ScheduledTransactionManager, "R6ClassGenerator"),
              message = "ScheduledTransactionManager class not available")

  manager <- ScheduledTransactionManager$new()

  sx1 <- monthly_scheduled("Rent", 2000, "acc1", "acc2",
                           start_date = as.Date("2024-01-01"))
  sx2 <- weekly_scheduled("Groceries", 200, "acc3", "acc2",
                          start_date = as.Date("2024-01-01"))

  manager$add(sx1)
  manager$add(sx2)

  df <- manager$as_tibble()

  expect_s3_class(df, "tbl_df")
  expect_equal(nrow(df), 2)
})
