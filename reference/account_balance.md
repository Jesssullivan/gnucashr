# Account Balance

Get the balance of a specific account as of a date.

## Usage

``` r
account_balance(gc, account, as_of = Sys.Date())
```

## Arguments

- gc:

  GnuCashDB object

- account:

  Account name, path, or GUID

- as_of:

  Date for balance calculation

## Value

Numeric balance
