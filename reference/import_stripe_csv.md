# Import Stripe Balance Transactions CSV

Imports transaction data from a Stripe balance transactions export.

## Usage

``` r
import_stripe_csv(path, include_fees = TRUE, timezone = "UTC")
```

## Arguments

- path:

  Path to the Stripe CSV file

- include_fees:

  If TRUE, include fee transactions separately (default TRUE)

- timezone:

  Timezone for date conversion (default "UTC")

## Value

A tibble with class `gnucashr_csv_import` containing standardized
columns

## Examples

``` r
if (FALSE) { # \dontrun{
stripe_data <- import_stripe_csv("stripe_balance.csv")
print(stripe_data)
} # }
```
