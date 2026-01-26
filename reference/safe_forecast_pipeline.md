# Safe Forecast Pipeline

Build and execute a forecast pipeline with full error handling. Each
step is validated and errors are captured as Results.

## Usage

``` r
safe_forecast_pipeline(source, ..., parallel = TRUE)
```

## Arguments

- source:

  Data source

- ...:

  Pipeline operations to apply

- parallel:

  Use parallel execution

## Value

Result with forecast output or error

## Examples

``` r
if (FALSE) { # \dontrun{
result <- safe_forecast_pipeline(
  config,
  lf_monte_carlo(n = 1000),
  lf_grow(rate = 0.05, months = 12)
)
} # }
```
