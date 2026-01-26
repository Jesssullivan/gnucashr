# Create a Quarterly Budget

Helper function to create a budget with quarterly periods.

## Usage

``` r
quarterly_budget(
  name,
  year = lubridate::year(Sys.Date()),
  description = NA_character_
)
```

## Arguments

- name:

  Budget name

- year:

  Budget year (default: current year)

- description:

  Optional description

## Value

A Budget object
