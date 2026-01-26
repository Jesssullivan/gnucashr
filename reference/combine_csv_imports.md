# Combine Multiple CSV Imports

Merges multiple gnucashr CSV import objects into a single tibble,
preserving metadata about the source of each transaction.

## Usage

``` r
combine_csv_imports(
  ...,
  deduplicate = FALSE,
  dedupe_key = c("date", "amount", "description")
)
```

## Arguments

- ...:

  Multiple `gnucashr_csv_import` objects

- deduplicate:

  If TRUE, attempt to remove duplicate transactions (default FALSE)

- dedupe_key:

  Columns to use for deduplication (default: date, amount, description)

## Value

A combined tibble with class `gnucashr_csv_import`

## Examples

``` r
if (FALSE) { # \dontrun{
paypal <- import_paypal_csv("paypal.csv")
stripe <- import_stripe_csv("stripe.csv")
combined <- combine_csv_imports(paypal, stripe)
} # }
```
