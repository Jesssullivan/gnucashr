# Commodity R6 Class

Commodity R6 Class

Commodity R6 Class

## Details

R6 class representing a GnuCash commodity (currency, stock, mutual fund,
etc.). Commodities are identified by their namespace and mnemonic (e.g.,
"CURRENCY:USD").

## Active bindings

- `guid`:

  Commodity GUID (read-only)

- `namespace`:

  Commodity namespace

- `mnemonic`:

  Commodity mnemonic

- `fullname`:

  Full descriptive name

- `cusip`:

  CUSIP/ISIN identifier

- `fraction`:

  Smallest unit (100 = cents)

- `quote_flag`:

  Whether to fetch online quotes

- `quote_source`:

  Source for online quotes

- `quote_tz`:

  Timezone for quotes

## Methods

### Public methods

- [`Commodity$new()`](#method-Commodity-new)

- [`Commodity$identifier()`](#method-Commodity-identifier)

- [`Commodity$is_currency()`](#method-Commodity-is_currency)

- [`Commodity$is_security()`](#method-Commodity-is_security)

- [`Commodity$decimal_places()`](#method-Commodity-decimal_places)

- [`Commodity$to_smallest_unit()`](#method-Commodity-to_smallest_unit)

- [`Commodity$from_smallest_unit()`](#method-Commodity-from_smallest_unit)

- [`Commodity$format_amount()`](#method-Commodity-format_amount)

- [`Commodity$as_tibble()`](#method-Commodity-as_tibble)

- [`Commodity$print()`](#method-Commodity-print)

- [`Commodity$clone()`](#method-Commodity-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Commodity object

#### Usage

    Commodity$new(
      guid = NULL,
      namespace = "CURRENCY",
      mnemonic,
      fullname = NA_character_,
      cusip = NA_character_,
      fraction = 100L,
      quote_flag = FALSE,
      quote_source = NA_character_,
      quote_tz = NA_character_
    )

#### Arguments

- `guid`:

  Unique identifier (32-character hex)

- `namespace`:

  Commodity namespace (CURRENCY, NASDAQ, NYSE, etc.)

- `mnemonic`:

  Short identifier (USD, AAPL, etc.)

- `fullname`:

  Full descriptive name

- `cusip`:

  CUSIP/ISIN identifier for securities

- `fraction`:

  Smallest unit (100 for USD = cents)

- `quote_flag`:

  Whether to fetch online quotes

- `quote_source`:

  Source for online quotes

- `quote_tz`:

  Timezone for quotes

------------------------------------------------------------------------

### Method `identifier()`

Get commodity identifier (namespace:mnemonic)

#### Usage

    Commodity$identifier()

------------------------------------------------------------------------

### Method `is_currency()`

Check if this is a currency

#### Usage

    Commodity$is_currency()

------------------------------------------------------------------------

### Method `is_security()`

Check if this is a security (stock, mutual fund, etc.)

#### Usage

    Commodity$is_security()

------------------------------------------------------------------------

### Method `decimal_places()`

Get number of decimal places

#### Usage

    Commodity$decimal_places()

------------------------------------------------------------------------

### Method `to_smallest_unit()`

Convert amount to smallest unit (e.g., dollars to cents)

#### Usage

    Commodity$to_smallest_unit(amount)

#### Arguments

- `amount`:

  Numeric amount to convert

------------------------------------------------------------------------

### Method `from_smallest_unit()`

Convert from smallest unit to decimal

#### Usage

    Commodity$from_smallest_unit(amount)

#### Arguments

- `amount`:

  Integer amount in smallest units

------------------------------------------------------------------------

### Method `format_amount()`

Format amount according to commodity precision

#### Usage

    Commodity$format_amount(amount, include_symbol = TRUE)

#### Arguments

- `amount`:

  Numeric amount

- `include_symbol`:

  Include currency symbol

------------------------------------------------------------------------

### Method `as_tibble()`

Convert to data frame row

#### Usage

    Commodity$as_tibble()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method

#### Usage

    Commodity$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Commodity$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
