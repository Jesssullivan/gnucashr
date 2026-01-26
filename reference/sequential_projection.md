# Sequential Projection (Non-Parallel Fallback)

Single-threaded projection for testing and comparison.

## Usage

``` r
sequential_projection(base_revenue, growth_rate, expense_rate, n_periods = 12L)
```

## Arguments

- base_revenue:

  Starting revenue

- growth_rate:

  Monthly growth rate

- expense_rate:

  Expense ratio

- n_periods:

  Number of periods

## Value

NumericVector of cumulative cash by period
