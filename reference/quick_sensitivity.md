# Quick Sensitivity Analysis

Convenience function for common sensitivity analysis pattern.

## Usage

``` r
quick_sensitivity(
  source,
  growth_range = seq(-0.02, 0.08, by = 0.01),
  expense_range = seq(0.5, 0.9, by = 0.05),
  months = 12
)
```

## Arguments

- source:

  Data source

- growth_range:

  Growth rate range

- expense_range:

  Expense ratio range

- months:

  Number of months

## Value

Sensitivity results
