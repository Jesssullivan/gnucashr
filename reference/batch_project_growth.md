# Batch Growth Projection

Project growth for many initial values with same parameters. Useful for
account-level projections.

## Usage

``` r
batch_project_growth(
  initial_values,
  growth_rate,
  expense_rate,
  n_periods = 12L
)
```

## Arguments

- initial_values:

  Vector of starting values

- growth_rate:

  Monthly growth rate

- expense_rate:

  Expense ratio (0-1)

- n_periods:

  Number of periods

## Value

List with projections matrix and final values
