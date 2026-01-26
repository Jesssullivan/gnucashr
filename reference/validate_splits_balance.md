# Validate Splits Balance

Verify that splits in a transaction sum to zero (double-entry
principle). Returns TRUE if balanced, FALSE otherwise.

## Usage

``` r
validate_splits_balance(numerators, denominators)
```

## Arguments

- numerators:

  Integer vector of split value_num

- denominators:

  Integer vector of split value_denom

## Value

Logical indicating if splits balance to zero
