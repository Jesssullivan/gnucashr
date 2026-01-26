# Audited Monte Carlo

Run Monte Carlo simulation with full audit logging.

## Usage

``` r
audited_monte_carlo(
  source,
  n = 10000,
  months = 12,
  growth_mean = 0.05,
  growth_sd = 0.03,
  seed = 42L,
  description = "Monte Carlo simulation"
)
```

## Arguments

- source:

  Data source

- n:

  Number of simulations

- months:

  Number of months

- growth_mean:

  Mean growth rate

- growth_sd:

  Growth standard deviation

- seed:

  Random seed

- description:

  Audit description

## Value

Logged Monte Carlo result

## Examples

``` r
if (FALSE) { # \dontrun{
result <- audited_monte_carlo(
  config,
  n = 10000,
  months = 12,
  description = "Annual cash flow projection for board meeting"
)

# Get results
data <- logged_value(result)
print(data$summary)

# Write audit log
write_audit_log(result, "forecast_audit.log")
} # }
```
