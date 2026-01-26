# ScheduledTransaction R6 Class

ScheduledTransaction R6 Class

ScheduledTransaction R6 Class

## Details

R6 class representing a GnuCash scheduled transaction. Scheduled
transactions define recurring transactions with a template.

## Active bindings

- `guid`:

  Transaction GUID (read-only)

- `name`:

  Transaction name

- `enabled`:

  Whether enabled

- `auto_create`:

  Auto-create flag

- `auto_notify`:

  Auto-notify flag

- `start_date`:

  Start date

- `end_date`:

  End date

- `last_occur`:

  Last occurrence date

- `num_occur`:

  Total occurrences

- `rem_occur`:

  Remaining occurrences

- `instance_count`:

  Instance count

- `recurrence_mult`:

  Recurrence multiplier

- `recurrence_period_type`:

  Recurrence period type

## Methods

### Public methods

- [`ScheduledTransaction$new()`](#method-ScheduledTransaction-new)

- [`ScheduledTransaction$set_recurrence()`](#method-ScheduledTransaction-set_recurrence)

- [`ScheduledTransaction$add_template_split()`](#method-ScheduledTransaction-add_template_split)

- [`ScheduledTransaction$template_splits()`](#method-ScheduledTransaction-template_splits)

- [`ScheduledTransaction$next_occurrence()`](#method-ScheduledTransaction-next_occurrence)

- [`ScheduledTransaction$recurrence_description()`](#method-ScheduledTransaction-recurrence_description)

- [`ScheduledTransaction$is_due()`](#method-ScheduledTransaction-is_due)

- [`ScheduledTransaction$total_amount()`](#method-ScheduledTransaction-total_amount)

- [`ScheduledTransaction$as_tibble()`](#method-ScheduledTransaction-as_tibble)

- [`ScheduledTransaction$print()`](#method-ScheduledTransaction-print)

- [`ScheduledTransaction$clone()`](#method-ScheduledTransaction-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new ScheduledTransaction object

#### Usage

    ScheduledTransaction$new(
      guid = NULL,
      name,
      enabled = TRUE,
      auto_create = FALSE,
      auto_notify = FALSE,
      advance_creation_days = 0L,
      advance_notify_days = 0L,
      start_date,
      end_date = NULL,
      last_occur = NULL,
      num_occur = 0L,
      rem_occur = 0L,
      instance_count = 0L,
      template_account_guid = NULL
    )

#### Arguments

- `guid`:

  Unique identifier (32-character hex)

- `name`:

  Transaction name/description

- `enabled`:

  Whether the scheduled transaction is active

- `auto_create`:

  Automatically create transactions

- `auto_notify`:

  Notify when transaction created

- `advance_creation_days`:

  Days in advance to create

- `advance_notify_days`:

  Days in advance to notify

- `start_date`:

  First occurrence date

- `end_date`:

  Optional end date

- `last_occur`:

  Last occurrence date

- `num_occur`:

  Total occurrences (0 = unlimited)

- `rem_occur`:

  Remaining occurrences

- `instance_count`:

  Number of instances created

- `template_account_guid`:

  GUID of template account holding splits

------------------------------------------------------------------------

### Method `set_recurrence()`

Set recurrence pattern

#### Usage

    ScheduledTransaction$set_recurrence(
      period_type,
      mult = 1L,
      weekend_adj = "none"
    )

#### Arguments

- `period_type`:

  Type: day, week, month, month_relative, year

- `mult`:

  Multiplier (e.g., 2 for every 2 weeks)

- `weekend_adj`:

  Weekend adjustment (none, forward, back)

------------------------------------------------------------------------

### Method `add_template_split()`

Add a template split

#### Usage

    ScheduledTransaction$add_template_split(
      account_guid,
      value_num,
      value_denom,
      memo = NA_character_
    )

#### Arguments

- `account_guid`:

  Target account GUID

- `value_num`:

  Value numerator

- `value_denom`:

  Value denominator

- `memo`:

  Optional memo

------------------------------------------------------------------------

### Method `template_splits()`

Get template splits

#### Usage

    ScheduledTransaction$template_splits()

------------------------------------------------------------------------

### Method `next_occurrence()`

Calculate next occurrence date

#### Usage

    ScheduledTransaction$next_occurrence()

------------------------------------------------------------------------

### Method `recurrence_description()`

Get human-readable recurrence description

#### Usage

    ScheduledTransaction$recurrence_description()

------------------------------------------------------------------------

### Method `is_due()`

Check if transaction is due

#### Usage

    ScheduledTransaction$is_due(as_of = Sys.Date())

#### Arguments

- `as_of`:

  Optional date to check against (default: today)

------------------------------------------------------------------------

### Method `total_amount()`

Get total amount from template splits

#### Usage

    ScheduledTransaction$total_amount()

------------------------------------------------------------------------

### Method `as_tibble()`

Convert to data frame row

#### Usage

    ScheduledTransaction$as_tibble()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method

#### Usage

    ScheduledTransaction$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ScheduledTransaction$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
