# Import PayPal Activity CSV Export

Imports transaction data from a PayPal activity export CSV file. PayPal
exports can have different column layouts; this function auto-detects
the format based on header columns.

## Usage

``` r
import_paypal_csv(path, include_pending = FALSE, timezone = "UTC")
```

## Arguments

- path:

  Path to the PayPal CSV file

- include_pending:

  Include transactions with "Pending" status (default FALSE)

- timezone:

  Timezone for date conversion (default "UTC")

## Value

A tibble with class `gnucashr_csv_import` containing standardized
columns

## Examples

``` r
if (FALSE) { # \dontrun{
paypal_data <- import_paypal_csv("paypal_activity.csv")
print(paypal_data)
} # }
```
