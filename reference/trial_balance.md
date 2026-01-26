# Generate Trial Balance

Calculate trial balance as of a specific date.

## Usage

``` r
trial_balance(gc, as_of = Sys.Date())
```

## Arguments

- gc:

  GnuCashDB object

- as_of:

  Date for balance calculation (default: today)

## Value

tibble with account, type, debit, credit, balance columns
