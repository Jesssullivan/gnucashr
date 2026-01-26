# Grow Revenue (Pipe-Friendly)

Add growth projection to a lazy forecast.

## Usage

``` r
lf_grow(lf, rate, months, compound = TRUE)
```

## Arguments

- lf:

  LazyForecast object

- rate:

  Monthly growth rate (e.g., 0.05 for 5%)

- months:

  Number of months to project

- compound:

  Use compound growth (default TRUE)

## Value

LazyForecast (for chaining)
