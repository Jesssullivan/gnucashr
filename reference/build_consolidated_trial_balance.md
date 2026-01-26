# Build Consolidated Trial Balance

Create fully consolidated trial balance with all adjustments.

## Usage

``` r
build_consolidated_trial_balance(books, ic_rules = list(), as_of = Sys.Date())
```

## Arguments

- books:

  List of GnuCashDB objects

- ic_rules:

  Intercompany rules

- as_of:

  Date for balances

## Value

Consolidated trial balance tibble
