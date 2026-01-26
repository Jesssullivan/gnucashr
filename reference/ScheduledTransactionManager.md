# ScheduledTransactionManager R6 Class

ScheduledTransactionManager R6 Class

ScheduledTransactionManager R6 Class

## Details

R6 class for managing scheduled transactions.

## Methods

### Public methods

- [`ScheduledTransactionManager$new()`](#method-ScheduledTransactionManager-new)

- [`ScheduledTransactionManager$add()`](#method-ScheduledTransactionManager-add)

- [`ScheduledTransactionManager$all()`](#method-ScheduledTransactionManager-all)

- [`ScheduledTransactionManager$due()`](#method-ScheduledTransactionManager-due)

- [`ScheduledTransactionManager$upcoming()`](#method-ScheduledTransactionManager-upcoming)

- [`ScheduledTransactionManager$as_tibble()`](#method-ScheduledTransactionManager-as_tibble)

- [`ScheduledTransactionManager$forecast_cash_flow()`](#method-ScheduledTransactionManager-forecast_cash_flow)

- [`ScheduledTransactionManager$print()`](#method-ScheduledTransactionManager-print)

- [`ScheduledTransactionManager$clone()`](#method-ScheduledTransactionManager-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new ScheduledTransactionManager

#### Usage

    ScheduledTransactionManager$new()

------------------------------------------------------------------------

### Method `add()`

Add a scheduled transaction

#### Usage

    ScheduledTransactionManager$add(sx)

#### Arguments

- `sx`:

  A ScheduledTransaction object

------------------------------------------------------------------------

### Method [`all()`](https://rdrr.io/r/base/all.html)

Get all scheduled transactions

#### Usage

    ScheduledTransactionManager$all(enabled_only = FALSE)

#### Arguments

- `enabled_only`:

  Only return enabled transactions

------------------------------------------------------------------------

### Method `due()`

Get due scheduled transactions

#### Usage

    ScheduledTransactionManager$due(as_of = Sys.Date())

#### Arguments

- `as_of`:

  Date to check (default: today)

------------------------------------------------------------------------

### Method `upcoming()`

Get upcoming scheduled transactions

#### Usage

    ScheduledTransactionManager$upcoming(days = 30L)

#### Arguments

- `days`:

  Number of days to look ahead

------------------------------------------------------------------------

### Method `as_tibble()`

Get all as tibble

#### Usage

    ScheduledTransactionManager$as_tibble()

------------------------------------------------------------------------

### Method `forecast_cash_flow()`

Calculate cash flow forecast from scheduled transactions

#### Usage

    ScheduledTransactionManager$forecast_cash_flow(
      months = 12L,
      account_mapping = list()
    )

#### Arguments

- `months`:

  Number of months to forecast

- `account_mapping`:

  Named list mapping account GUIDs to categories

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method

#### Usage

    ScheduledTransactionManager$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ScheduledTransactionManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
