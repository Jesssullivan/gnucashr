# Format Trial Balance as gt Table

Creates a formatted gt table from a trial balance tibble.

## Usage

``` r
gt_trial_balance(tb, title = NULL, currency = "$")
```

## Arguments

- tb:

  Trial balance tibble from trial_balance()

- title:

  Optional table title

- currency:

  Currency symbol (default "\$")

## Value

A gt table object

## Examples

``` r
if (FALSE) { # \dontrun{
gc <- read_gnucash("books.gnucash")
tb <- trial_balance(gc)
gt_trial_balance(tb, title = "Trial Balance")
} # }
```
