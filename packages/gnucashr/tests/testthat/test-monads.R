# Test monad implementations

# Result Monad Tests ----

test_that("ok creates success result", {
  result <- ok(42)

  expect_true(is_ok(result))
  expect_false(is_err(result))
  expect_equal(unwrap(result), 42)
})

test_that("err creates error result", {
  result <- err("Something went wrong")

  expect_false(is_ok(result))
  expect_true(is_err(result))
  expect_equal(unwrap_err(result), "Something went wrong")
})

test_that("unwrap throws for error result", {
  result <- err("Error message")

  expect_error(unwrap(result), "Attempted to unwrap an error result")
})

test_that("unwrap with default returns default for error", {
  result <- err("Error")

  expect_equal(unwrap(result, default = 0), 0)
})

test_that("result_map transforms ok value", {
  result <- ok(5) |>
    result_map(function(x) x * 2)

  expect_equal(unwrap(result), 10)
})

test_that("result_map passes through error", {
  result <- err("Error") |>
    result_map(function(x) x * 2)

  expect_true(is_err(result))
  expect_equal(unwrap_err(result), "Error")
})

test_that("result_bind chains operations", {
  result <- ok(5) |>
    result_bind(function(x) ok(x * 2)) |>
    result_bind(function(x) ok(x + 1))

  expect_equal(unwrap(result), 11)
})

test_that("result_bind short-circuits on error", {
  result <- ok(5) |>
    result_bind(function(x) err("Failed")) |>
    result_bind(function(x) ok(x + 1))

  expect_true(is_err(result))
  expect_equal(unwrap_err(result), "Failed")
})

test_that("result_match handles both cases", {
  ok_result <- ok(10)
  err_result <- err("Error")

  ok_value <- result_match(ok_result,
                           ok_fn = function(x) x * 2,
                           err_fn = function(e) 0)
  err_value <- result_match(err_result,
                            ok_fn = function(x) x * 2,
                            err_fn = function(e) -1)

  expect_equal(ok_value, 20)
  expect_equal(err_value, -1)
})

test_that("try_result captures success", {
  result <- try_result(1 + 1)

  expect_true(is_ok(result))
  expect_equal(unwrap(result), 2)
})

test_that("try_result captures error", {
  result <- try_result(stop("Test error"))

  expect_true(is_err(result))
  expect_true(grepl("Test error", unwrap_err(result)))
})

test_that("combine_results works for all ok", {
  results <- combine_results(ok(1), ok(2), ok(3))

  expect_true(is_ok(results))
  expect_equal(unwrap(results), list(1, 2, 3))
})

test_that("combine_results fails on any error", {
  results <- combine_results(ok(1), err("Fail"), ok(3))

  expect_true(is_err(results))
})

# Logger Monad Tests ----

test_that("logged wraps value with empty log", {
  result <- logged(42)

  expect_equal(logged_value(result), 42)
  expect_equal(logged_log(result), character())
})

test_that("logged wraps value with log entries", {
  result <- logged(42, c("Entry 1", "Entry 2"))

  expect_equal(logged_value(result), 42)
  expect_equal(length(logged_log(result)), 2)
})
test_that("log_append adds timestamped entry", {
  result <- logged(42) |>
    log_append("Test entry")

  log <- logged_log(result)
  expect_equal(length(log), 1)
  expect_true(grepl("Test entry", log[1]))
  expect_true(grepl("\\[\\d{4}-\\d{2}-\\d{2}", log[1]))  # Has timestamp
})

test_that("log_bind chains logged operations", {
  result <- logged(5) |>
    log_append("Start") |>
    log_bind(function(x) logged(x * 2, "Doubled")) |>
    log_bind(function(x) logged(x + 1, "Added one"))

  expect_equal(logged_value(result), 11)
  expect_equal(length(logged_log(result)), 3)
})

test_that("log_map transforms value without affecting log", {
  result <- logged(5, c("Entry")) |>
    log_map(function(x) x * 2)

  expect_equal(logged_value(result), 10)
  expect_equal(length(logged_log(result)), 1)
})
