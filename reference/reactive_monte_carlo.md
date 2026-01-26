# Create Reactive Monte Carlo Simulation

Creates a reactive Monte Carlo simulation with configurable parameters.

## Usage

``` r
reactive_monte_carlo(
  config_reactive,
  n_sims = 10000,
  growth_mean_reactive,
  growth_sd_reactive,
  seed = 42
)
```

## Arguments

- config_reactive:

  Reactive entity configuration

- n_sims:

  Number of simulations

- growth_mean_reactive:

  Reactive mean growth rate

- growth_sd_reactive:

  Reactive growth rate standard deviation

- seed:

  Random seed for reproducibility

## Value

A reactive expression returning simulation results
