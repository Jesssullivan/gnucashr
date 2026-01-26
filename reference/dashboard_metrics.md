# Create Value Boxes for Dashboard

Returns a list of key metrics formatted for Quarto value boxes.

## Usage

``` r
dashboard_metrics(gc, as_of = Sys.Date())
```

## Arguments

- gc:

  GnuCashDB object

- as_of:

  Date for metrics calculation

## Value

Named list with metrics (total_assets, total_liabilities, net_worth,
etc.)
