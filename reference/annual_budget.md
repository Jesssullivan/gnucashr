# Create an Annual Budget

Helper function to create a budget with a single annual period.

## Usage

``` r
annual_budget(
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
