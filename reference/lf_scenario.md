# Create Named Scenario (Pipe-Friendly)

Add a named scenario to a lazy forecast.

## Usage

``` r
lf_scenario(lf, name, growth_boost = 0, expense_change = 0, ...)
```

## Arguments

- lf:

  LazyForecast object

- name:

  Scenario name

- growth_boost:

  Growth rate adjustment

- expense_change:

  Expense rate adjustment

- ...:

  Additional scenario parameters

## Value

LazyForecast (for chaining)
