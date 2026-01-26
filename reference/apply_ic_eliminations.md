# Apply Intercompany Eliminations

Remove intercompany transactions from combined trial balance.

## Usage

``` r
apply_ic_eliminations(tb, ic_rules)
```

## Arguments

- tb:

  Combined trial balance tibble with book_name column

- ic_rules:

  List of IC elimination rules

## Value

tibble with IC transactions eliminated
