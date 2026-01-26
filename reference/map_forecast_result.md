# Map Result Over Forecasts

Apply a function to forecast results if Ok, propagate Err.

## Usage

``` r
map_forecast_result(result, fn)
```

## Arguments

- result:

  Result object from safe_collect

- fn:

  Function to apply to the Ok value

## Value

New Result with transformed value
