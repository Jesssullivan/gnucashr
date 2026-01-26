# Validate CSV Import

Check a CSV import for common issues like missing required fields,
invalid amounts, or suspicious duplicates.

## Usage

``` r
validate_csv_import(import, strict = FALSE)
```

## Arguments

- import:

  A `gnucashr_csv_import` object

- strict:

  If TRUE, return errors instead of warnings

## Value

List with validation results

## Examples

``` r
if (FALSE) { # \dontrun{
import <- import_paypal_csv("paypal.csv")
validation <- validate_csv_import(import)
} # }
```
