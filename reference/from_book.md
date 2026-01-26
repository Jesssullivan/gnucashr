# Create Forecast from GnuCash Book

Initialize a lazy forecast from a GnuCash database.

## Usage

``` r
from_book(gc)
```

## Arguments

- gc:

  GnuCashDB object

## Value

LazyForecast with GnuCashDB source

## Examples

``` r
if (FALSE) { # \dontrun{
gc <- read_gnucash("path/to/file.gnucash")
forecast <- from_book(gc) |>
  grow(rate = 0.05, months = 12)
} # }
```
