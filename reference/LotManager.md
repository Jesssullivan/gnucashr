# LotManager R6 Class

LotManager R6 Class

LotManager R6 Class

## Details

R6 class for managing lots for an account. Provides FIFO, LIFO, and
specific identification methods.

## Active bindings

- `account_guid`:

  Account GUID

- `method`:

  Cost basis method

## Methods

### Public methods

- [`LotManager$new()`](#method-LotManager-new)

- [`LotManager$add_lot()`](#method-LotManager-add_lot)

- [`LotManager$lots()`](#method-LotManager-lots)

- [`LotManager$open_lots_ordered()`](#method-LotManager-open_lots_ordered)

- [`LotManager$total_quantity()`](#method-LotManager-total_quantity)

- [`LotManager$total_cost_basis()`](#method-LotManager-total_cost_basis)

- [`LotManager$avg_cost()`](#method-LotManager-avg_cost)

- [`LotManager$unrealized_gain()`](#method-LotManager-unrealized_gain)

- [`LotManager$select_lots_for_sale()`](#method-LotManager-select_lots_for_sale)

- [`LotManager$as_tibble()`](#method-LotManager-as_tibble)

- [`LotManager$print()`](#method-LotManager-print)

- [`LotManager$clone()`](#method-LotManager-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new LotManager

#### Usage

    LotManager$new(account_guid, method = "FIFO")

#### Arguments

- `account_guid`:

  Account GUID

- `method`:

  Cost basis method (FIFO, LIFO, SPECIFIC)

------------------------------------------------------------------------

### Method `add_lot()`

Add a lot

#### Usage

    LotManager$add_lot(lot)

#### Arguments

- `lot`:

  A Lot object

------------------------------------------------------------------------

### Method `lots()`

Get all lots

#### Usage

    LotManager$lots(include_closed = TRUE)

#### Arguments

- `include_closed`:

  Include closed lots

------------------------------------------------------------------------

### Method `open_lots_ordered()`

Get open lots sorted by method order

#### Usage

    LotManager$open_lots_ordered()

------------------------------------------------------------------------

### Method `total_quantity()`

Calculate total quantity across all open lots

#### Usage

    LotManager$total_quantity()

------------------------------------------------------------------------

### Method `total_cost_basis()`

Calculate total cost basis across all open lots

#### Usage

    LotManager$total_cost_basis()

------------------------------------------------------------------------

### Method `avg_cost()`

Calculate weighted average cost

#### Usage

    LotManager$avg_cost()

------------------------------------------------------------------------

### Method `unrealized_gain()`

Calculate total unrealized gain

#### Usage

    LotManager$unrealized_gain(current_price)

#### Arguments

- `current_price`:

  Current market price

------------------------------------------------------------------------

### Method `select_lots_for_sale()`

Get lots to use for selling (based on method)

#### Usage

    LotManager$select_lots_for_sale(quantity)

#### Arguments

- `quantity`:

  Quantity to sell

------------------------------------------------------------------------

### Method `as_tibble()`

Get all lots as a tibble

#### Usage

    LotManager$as_tibble()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method

#### Usage

    LotManager$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LotManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
