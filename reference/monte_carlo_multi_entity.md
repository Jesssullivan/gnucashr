# Multi-Entity Parallel Monte Carlo

Run Monte Carlo simulation for multiple entities with correlated growth.

## Usage

``` r
monte_carlo_multi_entity(
  base_revenues,
  growth_means,
  growth_sds,
  expense_rates,
  n_sims = 10000L,
  n_periods = 12L,
  seed = 42L
)
```

## Arguments

- base_revenues:

  NumericVector of starting revenues per entity

- growth_means:

  NumericVector of mean growth rates per entity

- growth_sds:

  NumericVector of growth standard deviations per entity

- expense_rates:

  NumericVector of expense ratios per entity

- n_sims:

  Number of simulations (default 10000)

- n_periods:

  Number of periods (default 12)

- seed:

  Master seed for reproducibility

## Value

List with entity_results, total_cash, and summary
