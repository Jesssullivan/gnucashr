# Sensitivity Analysis (Pipe-Friendly)

Add sensitivity grid analysis to a lazy forecast.

## Usage

``` r
lf_sensitivity(
  lf,
  growth_range = seq(-0.02, 0.08, by = 0.01),
  expense_range = seq(0.5, 0.9, by = 0.05),
  months = 12
)
```

## Arguments

- lf:

  LazyForecast object

- growth_range:

  Vector of growth rates to test

- expense_range:

  Vector of expense ratios to test

- months:

  Number of months to project

## Value

LazyForecast (for chaining)

## Examples

``` r
if (FALSE) { # \dontrun{
forecast <- forecast_expr() |>
  lf_sensitivity(
    growth_range = seq(-0.02, 0.08, by = 0.01),
    expense_range = seq(0.5, 0.9, by = 0.05)
  )
result <- forecast$collect()
# result$outcomes is a matrix of results
} # }
```
