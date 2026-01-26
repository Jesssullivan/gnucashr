# Create a Price from Decimal Value

Helper function to create a price entry from a decimal value.

## Usage

``` r
new_price(
  commodity_guid,
  currency_guid,
  value,
  date = Sys.time(),
  source = "user:price",
  type = "last",
  precision = 4L
)
```

## Arguments

- commodity_guid:

  GUID of the commodity

- currency_guid:

  GUID of the currency

- value:

  Price as decimal value

- date:

  Price date (default: now)

- source:

  Price source (default: "user:price")

- type:

  Price type (default: "last")

- precision:

  Decimal places for storage (default: 4)

## Value

A Price object

## Examples

``` r
if (FALSE) { # \dontrun{
price <- new_price("abc123...", "def456...", 150.25)
} # }
```
