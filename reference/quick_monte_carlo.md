# Quick Monte Carlo Forecast

Convenience function for common Monte Carlo pattern.

## Usage

``` r
quick_monte_carlo(
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

  Data source (config, tibble, or path)

- n:

  Number of simulations

- months:

  Number of months

- growth_mean:

  Mean growth rate

- growth_sd:

  Growth rate standard deviation

- seed:

  Random seed

## Value

Monte Carlo results

## Examples

``` r
if (FALSE) { # \dontrun{
# Quick forecast from config
result <- quick_monte_carlo(config, n = 10000, months = 12)
result$summary$p50  # Median cumulative cash
} # }
```
