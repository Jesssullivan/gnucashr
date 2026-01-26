# Create Forecast Expression

Entry point for building a lazy forecast pipeline. The expression tree
is built without loading or computing data.

## Usage

``` r
forecast_expr(source = NULL)
```

## Arguments

- source:

  Optional data source (config, tibble, GnuCashDB, BookCollection)

## Value

LazyForecast object

## Examples

``` r
if (FALSE) { # \dontrun{
# Build expression tree - NO DATA LOADED
forecast <- forecast_expr() |>
  grow(rate = 0.05, months = 12) |>
  monte_carlo(n = 10000)

# Inspect without executing
forecast$show_plan()

# Execute and get results
result <- forecast$collect()
} # }
```
