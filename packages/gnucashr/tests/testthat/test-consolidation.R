# Test consolidation functions

test_that("create_standard_ic_rules generates rules", {
  rules <- create_standard_ic_rules(
    parent_name = "inc",
    subsidiary_names = c("products", "services")
  )

  expect_type(rules, "list")
  expect_true(length(rules) > 0)

  # Should have rules for each subsidiary
  rule_descriptions <- purrr::map_chr(rules, ~.x$description)
  expect_true(any(grepl("products", rule_descriptions)))
  expect_true(any(grepl("services", rule_descriptions)))
})

test_that("apply_ic_eliminations creates elimination entries", {
  # Mock combined trial balance
  tb <- tibble::tibble(
    book_name = c("parent", "parent", "sub", "sub"),
    account = c("Due from Sub", "Cash", "Due to Parent", "Revenue"),
    type = c("ASSET", "ASSET", "LIABILITY", "INCOME"),
    debit = c(1000, 5000, 0, 0),
    credit = c(0, 0, 1000, 2000),
    balance = c(1000, 5000, -1000, -2000)
  )

  ic_rules <- list(list(
    from_book = "parent",
    from_account = "Due from Sub",
    to_book = "sub",
    to_account = "Due to Parent",
    description = "IC test"
  ))

  result <- apply_ic_eliminations(tb, ic_rules)

  # Should have original 4 rows plus elimination entries
  expect_gt(nrow(result), 4)

  # Check elimination entries exist
  elim_rows <- result[result$book_name == "ELIMINATION", ]
  expect_gt(nrow(elim_rows), 0)
})

test_that("validate_consolidation checks balance", {
  # Balanced TB
  balanced_tb <- tibble::tibble(
    account = c("Assets", "Liabilities"),
    type = c("ASSET", "LIABILITY"),
    debit = c(1000, 0),
    credit = c(0, 1000),
    balance = c(1000, -1000)
  )

  validation <- validate_consolidation(balanced_tb)
  expect_true(validation$is_balanced)

  # Unbalanced TB
  unbalanced_tb <- tibble::tibble(
    account = c("Assets", "Liabilities"),
    type = c("ASSET", "LIABILITY"),
    debit = c(1000, 0),
    credit = c(0, 900),
    balance = c(1000, -900)
  )

  validation2 <- validate_consolidation(unbalanced_tb)
  expect_false(validation2$is_balanced)
})
