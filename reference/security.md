# Create a Stock/Security Commodity

Helper function to create a stock or security commodity.

## Usage

``` r
security(
  symbol,
  exchange = "NYSE",
  fullname = NA_character_,
  cusip = NA_character_,
  fraction = 10000L
)
```

## Arguments

- symbol:

  Ticker symbol (e.g., "AAPL", "GOOGL")

- exchange:

  Exchange namespace (NASDAQ, NYSE, etc.)

- fullname:

  Company/fund name

- cusip:

  CUSIP/ISIN identifier

- fraction:

  Price precision (default 10000 for 4 decimal places)

## Value

A Commodity object

## Examples

``` r
if (FALSE) { # \dontrun{
aapl <- security("AAPL", "NASDAQ", "Apple Inc.")
} # }
```
