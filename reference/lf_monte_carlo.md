# Monte Carlo Simulation (Pipe-Friendly)

Add Monte Carlo simulation to a lazy forecast.

## Usage

``` r
lf_monte_carlo(
  lf,
  n = 10000,
  growth_mean = 0.05,
  growth_sd = 0.03,
  expense_mean = 0.7,
  expense_sd = 0.05,
  months = 12,
  seed = 42L
)
```

## Arguments

- lf:

  LazyForecast object

- n:

  Number of simulations

- growth_mean:

  Mean monthly growth rate

- growth_sd:

  Growth rate standard deviation

- expense_mean:

  Mean expense ratio

- expense_sd:

  Expense ratio standard deviation

- months:

  Number of months to simulate

- seed:

  Random seed for reproducibility

## Value

LazyForecast (for chaining)

## Examples

``` r
if (FALSE) { # \dontrun{
forecast <- forecast_expr() |>
  lf_monte_carlo(
    n = 10000,
    growth_mean = 0.05,
    growth_sd = 0.03,
    seed = 42
  )
result <- forecast$collect()
result$summary$p50  # Median outcome
} # }
```
