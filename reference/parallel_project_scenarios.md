# Parallel Scenario Projection

Project multiple growth scenarios in parallel for each entity.

## Usage

``` r
parallel_project_scenarios(base_values, growth_matrix, n_periods = 12L)
```

## Arguments

- base_values:

  Starting values per entity

- growth_matrix:

  Matrix of growth rates (scenarios x entities)

- n_periods:

  Number of periods to project

## Value

List with results matrix, final totals, and metadata
