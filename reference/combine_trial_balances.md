# Combine Trial Balances from Multiple Books

Merge trial balances from multiple books with book identification.

## Usage

``` r
combine_trial_balances(books, as_of = Sys.Date())
```

## Arguments

- books:

  List of GnuCashDB objects (named)

- as_of:

  Date for balance calculation

## Value

Combined tibble with book_name column
