# Validate Transaction Balance

Check that a set of split values sum to zero (double-entry principle).
Uses exact integer arithmetic to avoid floating-point errors.

## Usage

``` r
validate_transaction_balance(value_nums, value_denoms, tolerance = 0)
```

## Arguments

- value_nums:

  Integer vector of split value numerators

- value_denoms:

  Integer vector of split value denominators

- tolerance:

  Maximum allowed imbalance (default 0, strict)

## Value

List with: balanced (bool), total (double), message (string)
