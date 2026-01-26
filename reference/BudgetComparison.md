# BudgetComparison R6 Class

BudgetComparison R6 Class

BudgetComparison R6 Class

## Details

R6 class for comparing budget to actuals.

## Active bindings

- `budget`:

  The budget being compared

## Methods

### Public methods

- [`BudgetComparison$new()`](#method-BudgetComparison-new)

- [`BudgetComparison$set_actual()`](#method-BudgetComparison-set_actual)

- [`BudgetComparison$get_actual()`](#method-BudgetComparison-get_actual)

- [`BudgetComparison$variance()`](#method-BudgetComparison-variance)

- [`BudgetComparison$variance_pct()`](#method-BudgetComparison-variance_pct)

- [`BudgetComparison$account_summary()`](#method-BudgetComparison-account_summary)

- [`BudgetComparison$as_tibble()`](#method-BudgetComparison-as_tibble)

- [`BudgetComparison$print()`](#method-BudgetComparison-print)

- [`BudgetComparison$clone()`](#method-BudgetComparison-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new BudgetComparison

#### Usage

    BudgetComparison$new(budget)

#### Arguments

- `budget`:

  A Budget object

------------------------------------------------------------------------

### Method `set_actual()`

Set actual amount for an account and period

#### Usage

    BudgetComparison$set_actual(account_guid, period_num, amount)

#### Arguments

- `account_guid`:

  Account GUID

- `period_num`:

  Period number

- `amount`:

  Actual amount

------------------------------------------------------------------------

### Method `get_actual()`

Get actual amount for an account and period

#### Usage

    BudgetComparison$get_actual(account_guid, period_num)

#### Arguments

- `account_guid`:

  Account GUID

- `period_num`:

  Period number

------------------------------------------------------------------------

### Method `variance()`

Get variance (actual - budget) for an account and period

#### Usage

    BudgetComparison$variance(account_guid, period_num)

#### Arguments

- `account_guid`:

  Account GUID

- `period_num`:

  Period number

------------------------------------------------------------------------

### Method `variance_pct()`

Get variance percentage for an account and period

#### Usage

    BudgetComparison$variance_pct(account_guid, period_num)

#### Arguments

- `account_guid`:

  Account GUID

- `period_num`:

  Period number

------------------------------------------------------------------------

### Method `account_summary()`

Get comparison summary for an account

#### Usage

    BudgetComparison$account_summary(account_guid)

#### Arguments

- `account_guid`:

  Account GUID

------------------------------------------------------------------------

### Method `as_tibble()`

Get full comparison as tibble

#### Usage

    BudgetComparison$as_tibble()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method

#### Usage

    BudgetComparison$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BudgetComparison$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
