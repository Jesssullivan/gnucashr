# Create Reactive Account Transactions

Creates a reactive list of transactions for a specific account.

## Usage

``` r
reactive_account_transactions(
  gc_reactive,
  account_reactive,
  start_reactive = NULL,
  end_reactive = NULL
)
```

## Arguments

- gc_reactive:

  Reactive GnuCashDB object

- account_reactive:

  Reactive account identifier (GUID or path)

- start_reactive:

  Optional reactive start date

- end_reactive:

  Optional reactive end date

## Value

A reactive expression returning transactions tibble
