# Parallel Monte Carlo Simulation

Run Monte Carlo simulation for financial projections using parallel
execution. Uses RcppParallel with thread-local RNG for reproducible,
deterministic results.

## Usage

``` r
monte_carlo_parallel(
  base_revenue,
  base_expense_rate,
  n_sims = 10000L,
  n_periods = 12L,
  growth_mean = 0.05,
  growth_sd = 0.03,
  expense_mean = -1,
  expense_sd = 0.05,
  seed = 42L
)
```

## Arguments

- base_revenue:

  Starting monthly revenue

- base_expense_rate:

  Base expense ratio (0-1)

- n_sims:

  Number of simulations to run (default 10000)

- n_periods:

  Number of periods to project (default 12)

- growth_mean:

  Mean monthly growth rate (default 0.05 = 5%)

- growth_sd:

  Standard deviation of growth (default 0.03)

- expense_mean:

  Mean expense ratio (default matches base_expense_rate)

- expense_sd:

  Standard deviation of expense ratio (default 0.05)

- seed:

  Master seed for reproducibility (default 42)

## Value

List with: results (matrix), final_cash (vector), summary (quantiles)
