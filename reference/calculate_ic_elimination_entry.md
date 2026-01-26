# Calculate IC Elimination Journal Entry

Generate the journal entry needed to eliminate a specific IC balance.

## Usage

``` r
calculate_ic_elimination_entry(collection, rule, as_of = Sys.Date())
```

## Arguments

- collection:

  BookCollection object

- rule:

  Single IC rule

- as_of:

  Date for balances

## Value

tibble representing the elimination journal entry
