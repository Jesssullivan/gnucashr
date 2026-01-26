# Lot R6 Class

Lot R6 Class

Lot R6 Class

## Details

R6 class representing a GnuCash lot for tracking cost basis and capital
gains/losses. Lots tie buy and sell transactions together for securities
tracking.

## Active bindings

- `guid`:

  Lot GUID (read-only)

- `account_guid`:

  Account GUID

- `is_closed`:

  Whether lot is closed

- `title`:

  Lot title

- `notes`:

  Lot notes

## Methods

### Public methods

- [`Lot$new()`](#method-Lot-new)

- [`Lot$add_split()`](#method-Lot-add_split)

- [`Lot$splits()`](#method-Lot-splits)

- [`Lot$quantity()`](#method-Lot-quantity)

- [`Lot$cost_basis()`](#method-Lot-cost_basis)

- [`Lot$proceeds()`](#method-Lot-proceeds)

- [`Lot$realized_gain()`](#method-Lot-realized_gain)

- [`Lot$avg_cost()`](#method-Lot-avg_cost)

- [`Lot$unrealized_gain()`](#method-Lot-unrealized_gain)

- [`Lot$open_date()`](#method-Lot-open_date)

- [`Lot$close_date()`](#method-Lot-close_date)

- [`Lot$holding_period()`](#method-Lot-holding_period)

- [`Lot$is_long_term()`](#method-Lot-is_long_term)

- [`Lot$as_tibble()`](#method-Lot-as_tibble)

- [`Lot$print()`](#method-Lot-print)

- [`Lot$clone()`](#method-Lot-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Lot object

#### Usage

    Lot$new(
      guid = NULL,
      account_guid,
      is_closed = FALSE,
      title = NA_character_,
      notes = NA_character_
    )

#### Arguments

- `guid`:

  Unique identifier (32-character hex)

- `account_guid`:

  GUID of the account this lot belongs to

- `is_closed`:

  Whether the lot is closed (fully sold)

- `title`:

  Optional title/name for the lot

- `notes`:

  Optional notes

------------------------------------------------------------------------

### Method `add_split()`

Add a split to this lot

#### Usage

    Lot$add_split(split_guid, quantity, value, date)

#### Arguments

- `split_guid`:

  GUID of the split

- `quantity`:

  Amount (positive for buy, negative for sell)

- `value`:

  Cost/proceeds in account currency

- `date`:

  Transaction date

------------------------------------------------------------------------

### Method `splits()`

Get all splits in this lot

#### Usage

    Lot$splits()

------------------------------------------------------------------------

### Method `quantity()`

Calculate total quantity in lot

#### Usage

    Lot$quantity()

------------------------------------------------------------------------

### Method `cost_basis()`

Calculate total cost basis

#### Usage

    Lot$cost_basis()

------------------------------------------------------------------------

### Method `proceeds()`

Calculate proceeds from sales

#### Usage

    Lot$proceeds()

------------------------------------------------------------------------

### Method `realized_gain()`

Calculate realized gain/loss

#### Usage

    Lot$realized_gain()

------------------------------------------------------------------------

### Method `avg_cost()`

Calculate average cost per unit

#### Usage

    Lot$avg_cost()

------------------------------------------------------------------------

### Method `unrealized_gain()`

Calculate unrealized gain/loss at a given price

#### Usage

    Lot$unrealized_gain(current_price)

#### Arguments

- `current_price`:

  Current market price per unit

------------------------------------------------------------------------

### Method `open_date()`

Get opening date (first buy)

#### Usage

    Lot$open_date()

------------------------------------------------------------------------

### Method `close_date()`

Get closing date (last sell that closed the lot)

#### Usage

    Lot$close_date()

------------------------------------------------------------------------

### Method `holding_period()`

Calculate holding period in days

#### Usage

    Lot$holding_period()

------------------------------------------------------------------------

### Method `is_long_term()`

Check if long-term (held \> 1 year)

#### Usage

    Lot$is_long_term()

------------------------------------------------------------------------

### Method `as_tibble()`

Convert to data frame row

#### Usage

    Lot$as_tibble()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method

#### Usage

    Lot$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Lot$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
