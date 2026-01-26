# Preview CSV Column Mapping

Debug helper to show how columns from a CSV file would be mapped by one
of the import functions.

## Usage

``` r
preview_csv_mapping(path, import_fn)
```

## Arguments

- path:

  Path to CSV file

- import_fn:

  Import function to use (e.g., import_paypal_csv)

## Value

A tibble showing column mappings and sample values

## Examples

``` r
if (FALSE) { # \dontrun{
# Preview how PayPal columns will be mapped
preview_csv_mapping("paypal.csv", import_paypal_csv)
} # }
```
