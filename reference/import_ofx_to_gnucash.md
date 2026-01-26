# Import OFX Transactions to GnuCash

Import OFX transactions directly into a GnuCash database. Creates
transactions with splits between the target account and a mapped or
default offset account.

## Usage

``` r
import_ofx_to_gnucash(
  ofx_data,
  gc,
  target_account,
  offset_account,
  skip_duplicates = TRUE,
  store_fitid = TRUE
)
```

## Arguments

- ofx_data:

  Tibble from
  [`import_ofx()`](https://tinyland.gitlab.io/projects/gnucashr/reference/import_ofx.md)
  or
  [`validate_ofx_import()`](https://tinyland.gitlab.io/projects/gnucashr/reference/validate_ofx_import.md)

- gc:

  A GnuCashDB connection (must be opened with read_only = FALSE)

- target_account:

  Account GUID or path for the bank account

- offset_account:

  Default offset account for transactions without mapping

- skip_duplicates:

  If TRUE (default), skip transactions marked as duplicates

- store_fitid:

  If TRUE (default), store FITID in memo for duplicate detection

## Value

Tibble with import results: original data plus tx_guid for created
transactions

## Examples

``` r
if (FALSE) { # \dontrun{
ofx <- import_ofx("statement.ofx")
gc <- read_gnucash("books.gnucash", read_only = FALSE)

result <- import_ofx_to_gnucash(
  ofx,
  gc,
  target_account = "Assets:Bank:Checking",
  offset_account = "Imbalance-USD"
)

gc$close()
} # }
```
