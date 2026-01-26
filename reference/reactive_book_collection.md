# Create Reactive Book Collection

Creates a reactive book collection for multi-entity dashboards.

## Usage

``` r
reactive_book_collection(paths_reactive, entity_config = NULL)
```

## Arguments

- paths_reactive:

  Reactive list of GnuCash file paths with names

- entity_config:

  Optional entity configuration

## Value

A reactive expression returning a BookCollection object
