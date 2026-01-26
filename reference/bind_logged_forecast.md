# Bind Logged Forecasts

Chain logged forecast operations, accumulating logs.

## Usage

``` r
bind_logged_forecast(logged_lf, fn)
```

## Arguments

- logged_lf:

  Logged object containing forecast or result

- fn:

  Function that takes a value and returns a logged object

## Value

New logged object with combined logs
