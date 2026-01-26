# Create a Currency Commodity

Helper function to create a currency commodity with standard settings.

## Usage

``` r
currency(code, fullname = NA_character_)
```

## Arguments

- code:

  ISO 4217 currency code (e.g., "USD", "EUR")

- fullname:

  Optional full name

## Value

A Commodity object

## Examples

``` r
if (FALSE) { # \dontrun{
usd <- currency("USD")
eur <- currency("EUR", "Euro")
} # }
```
