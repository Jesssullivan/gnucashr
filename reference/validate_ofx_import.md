# Validate OFX Import Data

Check imported OFX data for duplicates against existing transactions in
a GnuCash database. Uses FITID (external_id) for duplicate detection.

## Usage

``` r
validate_ofx_import(ofx_data, gc, account)
```

## Arguments

- ofx_data:

  Tibble from
  [`import_ofx()`](https://tinyland.gitlab.io/projects/gnucashr/reference/import_ofx.md)

- gc:

  A GnuCashDB connection

- account:

  Account GUID or path to check for duplicates

## Value

A tibble with the same structure as `ofx_data` plus columns:

- is_duplicate:

  Logical indicating if transaction already exists

- existing_tx_guid:

  GUID of matching transaction if duplicate

## Examples

``` r
if (FALSE) { # \dontrun{
ofx <- import_ofx("statement.ofx")
gc <- read_gnucash("books.gnucash")
validated <- validate_ofx_import(ofx, gc, "Assets:Bank:Checking")

# Filter to new transactions only
new_transactions <- validated |> dplyr::filter(!is_duplicate)
} # }
```
