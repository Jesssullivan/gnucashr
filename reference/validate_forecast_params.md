# Validate Forecast Parameters

Validate Monte Carlo or sensitivity parameters before execution.

## Usage

``` r
validate_forecast_params(
  n = 10000,
  months = 12,
  growth_mean = 0.05,
  growth_sd = 0.03
)
```

## Arguments

- n:

  Number of simulations (must be positive)

- months:

  Number of months (must be positive)

- growth_mean:

  Mean growth rate

- growth_sd:

  Growth standard deviation (must be non-negative)

## Value

Result with validated params or error
