# Parallel Sensitivity Grid

Compute sensitivity grid for growth rate and expense ratio combinations.
Efficient for large parameter sweeps.

## Usage

``` r
parallel_sensitivity_grid(
  base_revenue,
  base_expense_rate,
  growth_range,
  expense_range,
  n_periods = 12L
)
```

## Arguments

- base_revenue:

  Starting revenue

- base_expense_rate:

  Base expense ratio

- growth_range:

  Vector of growth rates to test

- expense_range:

  Vector of expense ratios to test

- n_periods:

  Number of periods to project

## Value

List with outcomes matrix and ranges
