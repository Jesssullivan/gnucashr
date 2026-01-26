# Create Forecast from Entity Config

Initialize a lazy forecast from entity configuration. Compatible with
functions_config.R load_entity_config() output.

## Usage

``` r
from_config(config)
```

## Arguments

- config:

  Entity configuration list

## Value

LazyForecast with config source

## Examples

``` r
if (FALSE) { # \dontrun{
config <- load_entity_config()
forecast <- from_config(config) |>
  grow(rate = 0.05, months = 12) |>
  monte_carlo(n = 10000)
} # }
```
