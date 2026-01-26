# Convert Double to Fraction

Convert decimal values to fractions with specified precision. Default
denominator of 100 for currency (cents precision).

## Usage

``` r
double_to_fraction(value, denom = 100L)
```

## Arguments

- value:

  NumericVector of decimal values

- denom:

  Target denominator (default 100 for cents)

## Value

IntegerMatrix with columns: numerator, denominator
