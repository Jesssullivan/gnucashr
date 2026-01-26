# Read a GnuCash File

Open a GnuCash file for reading. Automatically detects SQLite vs XML
format.

## Usage

``` r
read_gnucash(path, read_only = TRUE)
```

## Arguments

- path:

  Path to GnuCash file (.gnucash or .sqlite)

- read_only:

  Open in read-only mode (default TRUE)

## Value

A GnuCashDB object

## Examples

``` r
if (FALSE) { # \dontrun{
gc <- read_gnucash("path/to/books.gnucash")
gc$accounts()
gc$close()
} # }
```
