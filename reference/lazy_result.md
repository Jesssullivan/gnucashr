# Wrap LazyForecast with Result Monad

Create a function that collects a LazyForecast with error handling.

## Usage

``` r
lazy_result(lf)
```

## Arguments

- lf:

  LazyForecast object

## Value

Function that returns Result when called

## Examples

``` r
if (FALSE) { # \dontrun{
forecast <- forecast_expr(config) |>
  lf_monte_carlo(n = 1000)

# Wrap for safe execution
safe_fn <- lazy_result(forecast)
result <- safe_fn()

# Handle result
result_match(result,
  ok_fn = function(data) data$summary$p50,
  err_fn = function(e) warning(e)
)
} # }
```
