# test-forecast-lazy.R - Tests for LazyForecast R6 class

test_that("LazyForecast can be created empty", {
  lf <- lazy_forecast()
  expect_s3_class(lf, "LazyForecast")
  expect_true(is.null(lf$get_source()))
  expect_equal(length(lf$get_ast()), 0)
})

test_that("LazyForecast can be created from tibble", {
  data <- tibble::tibble(
    entity = c("products", "services"),
    revenue = c(50000, 70000),
    operating_rate = c(0.65, 0.70)
  )

  lf <- from_data(data)
  expect_s3_class(lf, "LazyForecast")
  expect_true(is.data.frame(lf$get_source()))
})

test_that("LazyForecast can be created from entity config", {
  config <- list(
    entities = list(
      products = list(revenue = 50000, operating_rate = 0.65),
      services = list(revenue = 70000, operating_rate = 0.70)
    ),
    growth = list(products = 0.05, services = 0.08)
  )

  lf <- from_config(config)
  expect_s3_class(lf, "LazyForecast")
  expect_true(is.list(lf$get_source()))
})

test_that("grow() builds AST without executing", {
  lf <- lazy_forecast() |>
    lf_grow(rate = 0.05, months = 12)

  # Should have one operation in AST
  ast <- lf$get_ast()
  expect_equal(length(ast), 1)
  expect_equal(ast[[1]]$op, "grow")
  expect_equal(ast[[1]]$rate, 0.05)
  expect_equal(ast[[1]]$months, 12)

  # Should not be materialized yet
  expect_false(lf$metadata()$materialized)
})

test_that("monte_carlo() builds AST without executing", {
  lf <- lazy_forecast() |>
    lf_monte_carlo(n = 1000, seed = 42)

  ast <- lf$get_ast()
  expect_equal(length(ast), 1)
  expect_equal(ast[[1]]$op, "monte_carlo")
  expect_equal(ast[[1]]$n, 1000)
  expect_equal(ast[[1]]$seed, 42)
  expect_true(ast[[1]]$parallel)
})

test_that("sensitivity() builds AST without executing", {
  lf <- lazy_forecast() |>
    lf_sensitivity(
      growth_range = seq(-0.02, 0.08, by = 0.02),
      expense_range = seq(0.5, 0.9, by = 0.1)
    )

  ast <- lf$get_ast()
  expect_equal(length(ast), 1)
  expect_equal(ast[[1]]$op, "sensitivity")
  expect_true(ast[[1]]$parallel)
})

test_that("operations can be chained", {
  lf <- lazy_forecast() |>
    lf_grow(rate = 0.05, months = 6) |>
    lf_monte_carlo(n = 500)

  ast <- lf$get_ast()
  expect_equal(length(ast), 2)
  expect_equal(ast[[1]]$op, "grow")
  expect_equal(ast[[2]]$op, "monte_carlo")
})

test_that("show_plan() displays AST correctly", {
  lf <- lazy_forecast() |>
    lf_grow(rate = 0.05, months = 12) |>
    lf_monte_carlo(n = 1000, seed = 42)

  # Capture output
  output <- capture.output(lf$show_plan())

  expect_true(any(grepl("LazyForecast Plan", output)))
  expect_true(any(grepl("grow", output)))
  expect_true(any(grepl("monte_carlo", output)))
  expect_true(any(grepl("Materialized: FALSE", output)))
})

test_that("metadata() returns correct information", {
  lf <- lazy_forecast() |>
    lf_grow(rate = 0.05, months = 12) |>
    lf_monte_carlo(n = 1000)

  meta <- lf$metadata()

  expect_equal(meta$n_operations, 2)
  expect_equal(meta$operations, c("grow", "monte_carlo"))
  expect_false(meta$materialized)
  expect_true(inherits(meta$created, "POSIXt"))
})

test_that("forecast_expr entry point works", {
  lf <- forecast_expr()
  expect_s3_class(lf, "LazyForecast")
})

test_that("pipe-friendly functions validate input", {
  # Non-LazyForecast input should error
  expect_error(lf_grow(list(), rate = 0.05, months = 12))
  expect_error(lf_monte_carlo("not a forecast"))
  expect_error(lf_collect(NULL))
})

test_that("from_book validates GnuCashDB input", {
  expect_error(from_book("not a db"), "GnuCashDB")
  expect_error(from_book(list()), "GnuCashDB")
})

test_that("from_collection validates BookCollection input", {
  expect_error(from_collection("not a collection"), "BookCollection")
})

test_that("from_config validates entity config input", {
  expect_error(from_config("not a config"), "entity configuration")
  expect_error(from_config(list(foo = 1)), "entity configuration")
})

test_that("print method works", {
  lf <- lazy_forecast() |>
    lf_grow(rate = 0.05, months = 12)

  # Should not error
  expect_output(print(lf), "LazyForecast")
})
