# Chain Forecast Results

Chain forecast operations, short-circuiting on error.

## Usage

``` r
bind_forecast_result(result, fn)
```

## Arguments

- result:

  Result object

- fn:

  Function that takes a value and returns a Result

## Value

Result from fn or the original error
