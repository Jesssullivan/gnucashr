# PriceDB R6 Class

PriceDB R6 Class

PriceDB R6 Class

## Details

R6 class for managing a collection of prices. Provides efficient lookup
of prices by commodity and date.

## Methods

### Public methods

- [`PriceDB$new()`](#method-PriceDB-new)

- [`PriceDB$add()`](#method-PriceDB-add)

- [`PriceDB$get_prices()`](#method-PriceDB-get_prices)

- [`PriceDB$get_price()`](#method-PriceDB-get_price)

- [`PriceDB$get_rate()`](#method-PriceDB-get_rate)

- [`PriceDB$convert()`](#method-PriceDB-convert)

- [`PriceDB$as_tibble()`](#method-PriceDB-as_tibble)

- [`PriceDB$count()`](#method-PriceDB-count)

- [`PriceDB$print()`](#method-PriceDB-print)

- [`PriceDB$clone()`](#method-PriceDB-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new PriceDB

#### Usage

    PriceDB$new(prices = list())

#### Arguments

- `prices`:

  Optional initial list of Price objects

------------------------------------------------------------------------

### Method `add()`

Add a price to the database

#### Usage

    PriceDB$add(price)

#### Arguments

- `price`:

  A Price object

------------------------------------------------------------------------

### Method `get_prices()`

Get all prices for a commodity

#### Usage

    PriceDB$get_prices(
      commodity_guid,
      currency_guid = NULL,
      start_date = NULL,
      end_date = NULL
    )

#### Arguments

- `commodity_guid`:

  GUID of the commodity

- `currency_guid`:

  Optional: filter by currency

- `start_date`:

  Optional: start of date range

- `end_date`:

  Optional: end of date range

------------------------------------------------------------------------

### Method `get_price()`

Get the most recent price for a commodity

#### Usage

    PriceDB$get_price(commodity_guid, currency_guid, as_of = NULL)

#### Arguments

- `commodity_guid`:

  GUID of the commodity

- `currency_guid`:

  GUID of the currency

- `as_of`:

  Optional: get price as of specific date

------------------------------------------------------------------------

### Method `get_rate()`

Get exchange rate between two commodities

#### Usage

    PriceDB$get_rate(from_guid, to_guid, as_of = NULL)

#### Arguments

- `from_guid`:

  Source commodity GUID

- `to_guid`:

  Target commodity GUID

- `as_of`:

  Optional: date for rate

------------------------------------------------------------------------

### Method `convert()`

Convert amount between commodities

#### Usage

    PriceDB$convert(amount, from_guid, to_guid, as_of = NULL)

#### Arguments

- `amount`:

  Amount to convert

- `from_guid`:

  Source commodity GUID

- `to_guid`:

  Target commodity GUID

- `as_of`:

  Optional: date for rate

------------------------------------------------------------------------

### Method `as_tibble()`

Get all prices as a tibble

#### Usage

    PriceDB$as_tibble()

------------------------------------------------------------------------

### Method `count()`

Get number of prices

#### Usage

    PriceDB$count()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method

#### Usage

    PriceDB$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PriceDB$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
