# Create Reactive Income Statement

Creates a reactive income statement for a date range. First generates
activity data, then creates the income statement from it.

## Usage

``` r
reactive_income_statement(gc_reactive, start_reactive, end_reactive)
```

## Arguments

- gc_reactive:

  Reactive GnuCashDB object

- start_reactive:

  Reactive start date

- end_reactive:

  Reactive end date

## Value

A reactive expression returning an income statement list
