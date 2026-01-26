# Create a New GnuCash SQLite Database

Create a new empty GnuCash file in SQLite format. Initializes with
minimal required structure (book, root account, currency).

## Usage

``` r
create_gnucash(path, currency = "USD", overwrite = FALSE)
```

## Arguments

- path:

  Path for the new GnuCash file

- currency:

  Default currency (default: "USD")

- overwrite:

  If TRUE, overwrite existing file (default: FALSE)

## Value

A GnuCashDB object opened for writing

## Examples

``` r
if (FALSE) { # \dontrun{
gc <- create_gnucash("path/to/new.gnucash")
create_account(gc, "Assets", "ASSET")
gc$close()
} # }
```
