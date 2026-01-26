# Create Forecast from Book Collection

Initialize a lazy forecast from a multi-book collection.

## Usage

``` r
from_collection(collection)
```

## Arguments

- collection:

  BookCollection object

## Value

LazyForecast with BookCollection source

## Examples

``` r
if (FALSE) { # \dontrun{
collection <- book_collection()
collection$add_book("inc", "path/to/inc.gnucash")
forecast <- from_collection(collection) |>
  grow(rate = 0.05, months = 12)
} # }
```
