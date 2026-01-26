# Convert Fractions to Double

Vectorized conversion of GnuCash fraction representation to double. This
is a hot path in balance calculations, hence Rcpp implementation.

## Usage

``` r
fraction_to_double(numerator, denominator)
```

## Arguments

- numerator:

  Integer vector of numerators (value_num)

- denominator:

  Integer vector of denominators (value_denom)

## Value

NumericVector of double values
