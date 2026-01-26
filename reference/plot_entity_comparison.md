# Create Entity Comparison Chart

Creates a ggplot2 bar chart comparing metrics across entities.

## Usage

``` r
plot_entity_comparison(
  collection,
  metric = "balance",
  as_of = Sys.Date(),
  title = NULL
)
```

## Arguments

- collection:

  BookCollection object

- metric:

  Metric to compare ("balance", "income", "expense")

- as_of:

  Date for comparison

- title:

  Optional plot title

## Value

A ggplot2 object
