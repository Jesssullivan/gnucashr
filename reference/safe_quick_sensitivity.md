# Safe Quick Sensitivity

Run quick_sensitivity with error handling.

## Usage

``` r
safe_quick_sensitivity(
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

Result with sensitivity output or error
