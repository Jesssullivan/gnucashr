# Create an Annual Scheduled Transaction

Helper function to create an annual recurring transaction.

## Usage

``` r
annual_scheduled(
  name,
  amount,
  debit_account_guid,
  credit_account_guid,
  start_date,
  auto_create = FALSE
)
```

## Arguments

- name:

  Transaction description

- amount:

  Transaction amount

- debit_account_guid:

  Account to debit

- credit_account_guid:

  Account to credit

- start_date:

  First occurrence

- auto_create:

  Auto-create transactions

## Value

A ScheduledTransaction object
