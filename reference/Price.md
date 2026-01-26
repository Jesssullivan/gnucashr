# Price R6 Class

Price R6 Class

Price R6 Class

## Details

R6 class representing a price/exchange rate between two commodities.
Used for converting between currencies and tracking security prices.

## Active bindings

- `guid`:

  Price GUID (read-only)

- `commodity_guid`:

  Commodity being priced

- `currency_guid`:

  Currency for pricing

- `date`:

  Price date/time

- `source`:

  Price source

- `type`:

  Price type (last, bid, ask, nav)

- `value_num`:

  Price numerator

- `value_denom`:

  Price denominator

## Methods

### Public methods

- [`Price$new()`](#method-Price-new)

- [`Price$value()`](#method-Price-value)

- [`Price$set_value()`](#method-Price-set_value)

- [`Price$convert_to_currency()`](#method-Price-convert_to_currency)

- [`Price$convert_to_commodity()`](#method-Price-convert_to_commodity)

- [`Price$as_tibble()`](#method-Price-as_tibble)

- [`Price$print()`](#method-Price-print)

- [`Price$clone()`](#method-Price-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Price object

#### Usage

    Price$new(
      guid = NULL,
      commodity_guid,
      currency_guid,
      date = Sys.time(),
      source = "user:price",
      type = "last",
      value_num,
      value_denom = 100L
    )

#### Arguments

- `guid`:

  Unique identifier (32-character hex)

- `commodity_guid`:

  GUID of the commodity being priced

- `currency_guid`:

  GUID of the currency used for pricing

- `date`:

  Date/time of the price

- `source`:

  Source of the price (user, yahoo, etc.)

- `type`:

  Price type (last, bid, ask, nav, etc.)

- `value_num`:

  Numerator of price fraction

- `value_denom`:

  Denominator of price fraction

------------------------------------------------------------------------

### Method `value()`

Get the price as a decimal value

#### Usage

    Price$value()

------------------------------------------------------------------------

### Method `set_value()`

Set the price from a decimal value

#### Usage

    Price$set_value(new_value, denom = 100L)

#### Arguments

- `new_value`:

  New price value

- `denom`:

  Denominator for storage precision

------------------------------------------------------------------------

### Method `convert_to_currency()`

Convert an amount from commodity to currency

#### Usage

    Price$convert_to_currency(commodity_amount)

#### Arguments

- `commodity_amount`:

  Amount in commodity units

------------------------------------------------------------------------

### Method `convert_to_commodity()`

Convert an amount from currency to commodity

#### Usage

    Price$convert_to_commodity(currency_amount)

#### Arguments

- `currency_amount`:

  Amount in currency units

------------------------------------------------------------------------

### Method `as_tibble()`

Convert to data frame row

#### Usage

    Price$as_tibble()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method

#### Usage

    Price$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Price$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
