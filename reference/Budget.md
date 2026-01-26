# Budget R6 Class

Budget R6 Class

Budget R6 Class

## Details

R6 class representing a GnuCash budget. Budgets have a defined period
structure and amounts per account.

## Active bindings

- `guid`:

  Budget GUID (read-only)

- `name`:

  Budget name

- `description`:

  Budget description

- `num_periods`:

  Number of periods

- `recurrence_period_type`:

  Period type

- `recurrence_mult`:

  Period multiplier

- `recurrence_start`:

  Start date

## Methods

### Public methods

- [`Budget$new()`](#method-Budget-new)

- [`Budget$set_amount()`](#method-Budget-set_amount)

- [`Budget$get_amount()`](#method-Budget-get_amount)

- [`Budget$get_account_amounts()`](#method-Budget-get_account_amounts)

- [`Budget$get_account_total()`](#method-Budget-get_account_total)

- [`Budget$get_period_dates()`](#method-Budget-get_period_dates)

- [`Budget$period_labels()`](#method-Budget-period_labels)

- [`Budget$budgeted_accounts()`](#method-Budget-budgeted_accounts)

- [`Budget$amounts_as_tibble()`](#method-Budget-amounts_as_tibble)

- [`Budget$as_tibble()`](#method-Budget-as_tibble)

- [`Budget$print()`](#method-Budget-print)

- [`Budget$clone()`](#method-Budget-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Budget object

#### Usage

    Budget$new(
      guid = NULL,
      name,
      description = NA_character_,
      num_periods = 12L,
      recurrence_period_type = "month",
      recurrence_mult = 1L,
      recurrence_start = NULL
    )

#### Arguments

- `guid`:

  Unique identifier (32-character hex)

- `name`:

  Budget name

- `description`:

  Optional description

- `num_periods`:

  Number of budget periods (12 for monthly, 4 for quarterly)

- `recurrence_period_type`:

  Period type (month, quarter, year)

- `recurrence_mult`:

  Multiplier for period

- `recurrence_start`:

  Start date for budget periods

------------------------------------------------------------------------

### Method `set_amount()`

Set budget amount for an account and period

#### Usage

    Budget$set_amount(account_guid, period_num, amount)

#### Arguments

- `account_guid`:

  Account GUID

- `period_num`:

  Period number (1-indexed)

- `amount`:

  Budget amount

------------------------------------------------------------------------

### Method `get_amount()`

Get budget amount for an account and period

#### Usage

    Budget$get_amount(account_guid, period_num)

#### Arguments

- `account_guid`:

  Account GUID

- `period_num`:

  Period number (1-indexed)

------------------------------------------------------------------------

### Method `get_account_amounts()`

Get all budget amounts for an account

#### Usage

    Budget$get_account_amounts(account_guid)

#### Arguments

- `account_guid`:

  Account GUID

------------------------------------------------------------------------

### Method `get_account_total()`

Get total budget for an account across all periods

#### Usage

    Budget$get_account_total(account_guid)

#### Arguments

- `account_guid`:

  Account GUID

------------------------------------------------------------------------

### Method `get_period_dates()`

Get date range for a period

#### Usage

    Budget$get_period_dates(period_num)

#### Arguments

- `period_num`:

  Period number (1-indexed)

------------------------------------------------------------------------

### Method `period_labels()`

Get all period labels

#### Usage

    Budget$period_labels()

------------------------------------------------------------------------

### Method `budgeted_accounts()`

Get all accounts with budget amounts

#### Usage

    Budget$budgeted_accounts()

------------------------------------------------------------------------

### Method `amounts_as_tibble()`

Get budget amounts as a tibble

#### Usage

    Budget$amounts_as_tibble()

------------------------------------------------------------------------

### Method `as_tibble()`

Convert to data frame row

#### Usage

    Budget$as_tibble()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method

#### Usage

    Budget$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Budget$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
