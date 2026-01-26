# Create a Monthly Budget

Helper function to create a budget with monthly periods.

## Usage

``` r
monthly_budget(
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

## Examples

``` r
if (FALSE) { # \dontrun{
budget <- monthly_budget("2024 Operating Budget", 2024)
} # }
```
