# Create a Weekly Scheduled Transaction

Helper function to create a weekly recurring transaction.

## Usage

``` r
weekly_scheduled(
  name,
  amount,
  debit_account_guid,
  credit_account_guid,
  start_date,
  every_n_weeks = 1L,
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

- every_n_weeks:

  Recurrence (1 = every week, 2 = biweekly)

- auto_create:

  Auto-create transactions

## Value

A ScheduledTransaction object
