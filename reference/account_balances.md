# Account Balances for Multiple Accounts

Get balances for multiple accounts efficiently.

## Usage

``` r
account_balances(gc, accounts, as_of = Sys.Date())
```

## Arguments

- gc:

  GnuCashDB object

- accounts:

  Character vector of account names/paths/GUIDs

- as_of:

  Date for balance calculation

## Value

Named numeric vector of balances
