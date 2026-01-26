# Audited Sensitivity Analysis

Run sensitivity analysis with full audit logging.

## Usage

``` r
audited_sensitivity(
  source,
  growth_range = seq(-0.02, 0.08, by = 0.01),
  expense_range = seq(0.5, 0.9, by = 0.05),
  months = 12,
  description = "Sensitivity analysis"
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

- description:

  Audit description

## Value

Logged sensitivity result
