# Safe Quick Monte Carlo

Run quick_monte_carlo with error handling.

## Usage

``` r
safe_quick_monte_carlo(
  source,
  n = 10000,
  months = 12,
  growth_mean = 0.05,
  growth_sd = 0.03,
  seed = 42L
)
```

## Arguments

- source:

  Data source

- n:

  Number of simulations

- months:

  Number of months

- growth_mean:

  Mean growth rate

- growth_sd:

  Growth standard deviation

- seed:

  Random seed

## Value

Result with Monte Carlo output or error
