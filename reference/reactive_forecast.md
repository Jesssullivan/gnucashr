# Create Reactive Forecast

Creates a reactive forecast that recalculates when parameters change.

## Usage

``` r
reactive_forecast(config_reactive, growth_reactive, months = 12)
```

## Arguments

- config_reactive:

  Reactive entity configuration

- growth_reactive:

  Reactive growth rate

- months:

  Number of months to forecast

## Value

A reactive expression returning forecast data
