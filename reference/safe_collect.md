# Safe Collect with Result Monad

Execute a LazyForecast with error handling. Returns Ok(result) on
success or Err(message) on failure.

## Usage

``` r
safe_collect(lf, parallel = TRUE)
```

## Arguments

- lf:

  LazyForecast object

- parallel:

  Use parallel execution

## Value

Result object (Ok or Err)

## Examples

``` r
if (FALSE) { # \dontrun{
forecast <- forecast_expr(config) |>
  lf_monte_carlo(n = 1000)

result <- safe_collect(forecast)

if (is_ok(result)) {
  data <- unwrap(result)
  print(data$summary)
} else {
  warning("Forecast failed: ", unwrap_err(result))
}
} # }
```
