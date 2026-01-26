# Calculate Running Balance

Calculate running balance for a series of transactions. Optimized for
large transaction histories.

## Usage

``` r
calculate_running_balance(value_nums, value_denoms, opening_balance = 0)
```

## Arguments

- value_nums:

  Integer vector of value numerators

- value_denoms:

  Integer vector of value denominators

- opening_balance:

  Starting balance (default 0)

## Value

NumericVector of running balances
