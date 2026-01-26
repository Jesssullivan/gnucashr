# Audited Consolidation

Perform multi-book consolidation with audit logging.

## Usage

``` r
audited_consolidation(collection, as_of = Sys.Date(), eliminate_ic = TRUE)
```

## Arguments

- collection:

  BookCollection object

- as_of:

  Date for balances

- eliminate_ic:

  Apply IC eliminations

## Value

Logged consolidated trial balance
