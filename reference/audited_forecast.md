# Create Audited Forecast

Wrap a LazyForecast with audit logging.

## Usage

``` r
audited_forecast(lf, description = "Financial forecast")
```

## Arguments

- lf:

  LazyForecast object

- description:

  Description of the forecast purpose

## Value

Logged object containing LazyForecast

## Examples

``` r
if (FALSE) { # \dontrun{
forecast <- forecast_expr(config) |>
  lf_monte_carlo(n = 1000)

audited <- audited_forecast(forecast, "Q1 2026 cash flow projection")
} # }
```
