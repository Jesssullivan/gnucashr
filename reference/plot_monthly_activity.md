# Create Monthly Activity Line Chart

Creates a ggplot2 line chart showing monthly account activity.

## Usage

``` r
plot_monthly_activity(activity, account_filter = NULL, title = NULL)
```

## Arguments

- activity:

  Monthly activity tibble from monthly_activity()

- account_filter:

  Optional vector of account names to include

- title:

  Optional plot title

## Value

A ggplot2 object
