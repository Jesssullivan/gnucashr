# Get Account Transactions

Retrieve transactions for a specific account.

## Usage

``` r
account_transactions(gc, account, start = NULL, end = NULL)
```

## Arguments

- gc:

  GnuCashDB object

- account:

  Account name or path

- start:

  Start date (optional)

- end:

  End date (optional)

## Value

tibble of transactions
