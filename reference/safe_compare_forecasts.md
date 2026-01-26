# Safe Comparison of Forecasts

Compare multiple forecasts with error handling.

## Usage

``` r
safe_compare_forecasts(..., parallel = TRUE)
```

## Arguments

- ...:

  Named LazyForecast objects

- parallel:

  Use parallel execution

## Value

Result with comparison tibble or error

## Examples

``` r
if (FALSE) { # \dontrun{
result <- safe_compare_forecasts(
  base = forecast_expr(config) |> lf_monte_carlo(n = 1000),
  optimistic = forecast_expr(config) |> lf_monte_carlo(n = 1000, growth_mean = 0.08)
)

result_match(result,
  ok_fn = print,
  err_fn = function(e) warning("Comparison failed: ", e)
)
} # }
```
