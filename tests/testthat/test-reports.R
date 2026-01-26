# Test financial reports

test_that("balance_sheet creates proper structure", {
  # Create a mock trial balance
  tb <- tibble::tibble(
    account = c("Checking", "Accounts Payable", "Retained Earnings", "Sales", "Rent"),
    type = c("BANK", "PAYABLE", "EQUITY", "INCOME", "EXPENSE"),
    debit = c(10000, 0, 0, 0, 2000),
    credit = c(0, 3000, 5000, 4000, 0),
    balance = c(10000, -3000, -5000, -4000, 2000)
  )

  bs <- balance_sheet(tb)

  expect_s3_class(bs, "data.frame")
  expect_true("section" %in% names(bs))
  expect_true("ASSETS" %in% bs$section)
  expect_true("LIABILITIES" %in% bs$section)
  expect_true("EQUITY" %in% bs$section)
})

test_that("income_statement creates proper structure", {
  # Create mock income/expense data
  activity <- tibble::tibble(
    account = c("Sales", "Service Revenue", "Rent", "Utilities"),
    type = c("INCOME", "INCOME", "EXPENSE", "EXPENSE"),
    balance = c(-10000, -5000, 3000, 1000),  # Income is negative (credit)
    debit = c(0, 0, 3000, 1000),
    credit = c(10000, 5000, 0, 0)
  )

  is <- income_statement(activity)

  expect_s3_class(is, "data.frame")
  expect_true("section" %in% names(is))
  expect_true("REVENUE" %in% is$section)
  expect_true("EXPENSES" %in% is$section)

  # Check attributes
  expect_equal(attr(is, "total_revenue"), 15000)
  expect_equal(attr(is, "total_expenses"), 4000)
  expect_equal(attr(is, "net_income"), 11000)
})

test_that("aggregate_by_type works correctly", {
  tb <- tibble::tibble(
    account = c("Checking", "Savings", "AP", "AR"),
    type = c("BANK", "BANK", "PAYABLE", "RECEIVABLE"),
    debit = c(1000, 2000, 0, 500),
    credit = c(0, 0, 1000, 0),
    balance = c(1000, 2000, -1000, 500)
  )

  agg <- aggregate_by_type(tb)

  expect_equal(nrow(agg), 3)  # BANK, PAYABLE, RECEIVABLE
  bank_row <- agg[agg$type == "BANK", ]
  expect_equal(bank_row$balance, 3000)
  expect_equal(bank_row$n_accounts, 2)
})
