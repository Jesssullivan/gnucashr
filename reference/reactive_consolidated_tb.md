# Create Reactive Consolidated Trial Balance

Creates a reactive consolidated trial balance across multiple books.

## Usage

``` r
reactive_consolidated_tb(
  collection_reactive,
  as_of_reactive = NULL,
  eliminate_ic = TRUE
)
```

## Arguments

- collection_reactive:

  Reactive BookCollection object

- as_of_reactive:

  Reactive date

- eliminate_ic:

  Whether to eliminate intercompany transactions

## Value

A reactive expression returning consolidated trial balance
