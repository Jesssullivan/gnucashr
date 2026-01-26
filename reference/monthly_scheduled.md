# Create a Monthly Scheduled Transaction

Helper function to create a monthly recurring transaction.

## Usage

``` r
monthly_scheduled(
  name,
  amount,
  debit_account_guid,
  credit_account_guid,
  start_date,
  day_of_month = NULL,
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

- day_of_month:

  Day of month (1-31, or "last")

- auto_create:

  Auto-create transactions

## Value

A ScheduledTransaction object
