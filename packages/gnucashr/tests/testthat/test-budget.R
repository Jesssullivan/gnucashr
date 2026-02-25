# test-budget.R - Tests for Budget R6 class

test_that("Budget can be created", {
  skip_if_not(inherits(Budget, "R6ClassGenerator"),
              message = "Budget class not available")

  budget <- Budget$new(
    name = "2024 Budget",
    num_periods = 12L,
    recurrence_period_type = "month"
  )

  expect_s3_class(budget, "Budget")
  expect_equal(budget$name, "2024 Budget")
  expect_equal(budget$num_periods, 12L)
})

test_that("monthly_budget helper creates monthly budget", {
  skip_if_not(exists("monthly_budget"),
              message = "monthly_budget function not available")

  budget <- monthly_budget("Test Budget", 2024)

  expect_s3_class(budget, "Budget")
  expect_equal(budget$num_periods, 12L)
  expect_equal(budget$recurrence_period_type, "month")
})

test_that("quarterly_budget helper creates quarterly budget", {
  skip_if_not(exists("quarterly_budget"),
              message = "quarterly_budget function not available")

  budget <- quarterly_budget("Q Budget", 2024)

  expect_s3_class(budget, "Budget")
  expect_equal(budget$num_periods, 4L)
})

test_that("Budget set_amount and get_amount work", {
  skip_if_not(inherits(Budget, "R6ClassGenerator"),
              message = "Budget class not available")

  budget <- monthly_budget("Test", 2024)

  budget$set_amount("acc123", 1, 1000)
  budget$set_amount("acc123", 2, 1200)

  expect_equal(budget$get_amount("acc123", 1), 1000)
  expect_equal(budget$get_amount("acc123", 2), 1200)
  expect_equal(budget$get_amount("acc123", 3), 0)  # Not set
})

test_that("Budget get_account_amounts returns all periods", {
  skip_if_not(inherits(Budget, "R6ClassGenerator"),
              message = "Budget class not available")

  budget <- monthly_budget("Test", 2024)

  for (p in 1:12) {
    budget$set_amount("acc123", p, p * 100)
  }

  amounts <- budget$get_account_amounts("acc123")

  expect_length(amounts, 12)
  expect_equal(amounts[1], 100)
  expect_equal(amounts[12], 1200)
})

test_that("Budget get_account_total sums all periods", {
  skip_if_not(inherits(Budget, "R6ClassGenerator"),
              message = "Budget class not available")

  budget <- monthly_budget("Test", 2024)

  for (p in 1:12) {
    budget$set_amount("acc123", p, 1000)  # $1000/month
  }

  total <- budget$get_account_total("acc123")
  expect_equal(total, 12000)
})

test_that("Budget period_labels returns formatted labels", {
  skip_if_not(inherits(Budget, "R6ClassGenerator"),
              message = "Budget class not available")

  budget <- monthly_budget("Test", 2024)

  labels <- budget$period_labels()

  expect_length(labels, 12)
  expect_equal(labels[1], "Jan 2024")
  expect_equal(labels[12], "Dec 2024")
})

test_that("Budget get_period_dates returns date range", {
  skip_if_not(inherits(Budget, "R6ClassGenerator"),
              message = "Budget class not available")

  budget <- monthly_budget("Test", 2024)

  dates <- budget$get_period_dates(1)

  expect_true("start" %in% names(dates))
  expect_true("end" %in% names(dates))
  expect_equal(format(dates$start, "%Y-%m-%d"), "2024-01-01")
})

test_that("Budget budgeted_accounts returns unique accounts", {
  skip_if_not(inherits(Budget, "R6ClassGenerator"),
              message = "Budget class not available")

  budget <- monthly_budget("Test", 2024)

  budget$set_amount("acc1", 1, 1000)
  budget$set_amount("acc1", 2, 1000)
  budget$set_amount("acc2", 1, 500)

  accounts <- budget$budgeted_accounts()

  expect_length(accounts, 2)
  expect_true("acc1" %in% accounts)
  expect_true("acc2" %in% accounts)
})

test_that("Budget as_tibble returns correct structure", {
  skip_if_not(inherits(Budget, "R6ClassGenerator"),
              message = "Budget class not available")

  budget <- monthly_budget("Test", 2024, "Test description")
  df <- budget$as_tibble()

  expect_s3_class(df, "tbl_df")
  expect_true(all(c("guid", "name", "num_periods") %in% names(df)))
  expect_equal(nrow(df), 1)
})

test_that("Budget amounts_as_tibble returns all amounts", {
  skip_if_not(inherits(Budget, "R6ClassGenerator"),
              message = "Budget class not available")

  budget <- monthly_budget("Test", 2024)

  budget$set_amount("acc1", 1, 1000)
  budget$set_amount("acc1", 2, 1200)
  budget$set_amount("acc2", 1, 500)

  df <- budget$amounts_as_tibble()

  expect_s3_class(df, "tbl_df")
  expect_equal(nrow(df), 3)
  expect_true(all(c("account_guid", "period_num", "amount") %in% names(df)))
})

test_that("BudgetComparison calculates variance", {
  skip_if_not(inherits(BudgetComparison, "R6ClassGenerator"),
              message = "BudgetComparison class not available")

  budget <- monthly_budget("Test", 2024)
  budget$set_amount("acc1", 1, 1000)

  comparison <- BudgetComparison$new(budget)
  comparison$set_actual("acc1", 1, 1100)

  variance <- comparison$variance("acc1", 1)
  expect_equal(variance, 100)  # actual - budget

  variance_pct <- comparison$variance_pct("acc1", 1)
  expect_equal(variance_pct, 10)  # 10% over budget
})

test_that("BudgetComparison account_summary returns all metrics", {
  skip_if_not(inherits(BudgetComparison, "R6ClassGenerator"),
              message = "BudgetComparison class not available")

  budget <- monthly_budget("Test", 2024)
  budget$set_amount("acc1", 1, 1000)
  budget$set_amount("acc1", 2, 1000)

  comparison <- BudgetComparison$new(budget)
  comparison$set_actual("acc1", 1, 1100)
  comparison$set_actual("acc1", 2, 900)

  summary <- comparison$account_summary("acc1")

  expect_s3_class(summary, "tbl_df")
  expect_equal(nrow(summary), 12)
  expect_true(all(c("period", "budget", "actual", "variance") %in% names(summary)))
})
