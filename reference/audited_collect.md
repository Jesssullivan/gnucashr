# Audited Collect

Execute a LazyForecast with full audit logging. Records execution time,
parameters, and result summary.

## Usage

``` r
audited_collect(audited_lf, parallel = TRUE)
```

## Arguments

- audited_lf:

  Logged object containing LazyForecast

- parallel:

  Use parallel execution

## Value

Logged object with materialized result

## Examples

``` r
if (FALSE) { # \dontrun{
forecast <- forecast_expr(config) |>
  lf_monte_carlo(n = 1000)

audited <- audited_forecast(forecast, "Monthly projection")
result <- audited_collect(audited)

# Get result data
data <- logged_value(result)

# Get audit log
log <- logged_log(result)
} # }
```
