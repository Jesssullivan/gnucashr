# Extract Quantile from Result

Safely extract a specific quantile from Monte Carlo result.

## Usage

``` r
extract_quantile(result, quantile = "p50", default = NA_real_)
```

## Arguments

- result:

  Result from safe_collect

- quantile:

  Quantile name (e.g., "p50", "p95")

- default:

  Default value if extraction fails

## Value

Numeric quantile value or default
