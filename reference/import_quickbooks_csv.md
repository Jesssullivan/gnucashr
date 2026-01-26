# Import QuickBooks Transaction CSV

Imports transaction data from QuickBooks Online or Desktop export.
Auto-detects the format from header columns.

## Usage

``` r
import_quickbooks_csv(
  path,
  format = c("auto", "qbo", "desktop"),
  timezone = Sys.timezone()
)
```

## Arguments

- path:

  Path to the QuickBooks CSV file

- format:

  One of "auto", "qbo" (QuickBooks Online), or "desktop"

- timezone:

  Timezone for date conversion (default from system)

## Value

A tibble with class `gnucashr_csv_import` containing standardized
columns

## Examples

``` r
if (FALSE) { # \dontrun{
qb_data <- import_quickbooks_csv("qb_transactions.csv")
print(qb_data)
} # }
```
