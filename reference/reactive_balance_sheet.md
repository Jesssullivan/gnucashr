# Create Reactive Balance Sheet

Creates a reactive balance sheet that updates when the underlying
GnuCash data changes. First generates a trial balance, then creates the
balance sheet from it.

## Usage

``` r
reactive_balance_sheet(gc_reactive, as_of_reactive = NULL)
```

## Arguments

- gc_reactive:

  Reactive GnuCashDB object

- as_of_reactive:

  Reactive date (optional)

## Value

A reactive expression returning a balance sheet list
