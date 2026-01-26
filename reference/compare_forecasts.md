# Compare Multiple Forecasts

Run and compare multiple lazy forecasts.

## Usage

``` r
compare_forecasts(..., parallel = TRUE)
```

## Arguments

- ...:

  Named LazyForecast objects to compare

- parallel:

  Use parallel execution

## Value

Comparison tibble

## Examples

``` r
if (FALSE) { # \dontrun{
base <- forecast_expr(config) |> lf_monte_carlo(n = 1000)
optimistic <- forecast_expr(config) |>
  lf_monte_carlo(n = 1000, growth_mean = 0.08)

compare_forecasts(
  base = base,
  optimistic = optimistic
)
} # }
```
