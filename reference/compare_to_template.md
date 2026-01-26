# Compare Template to Existing Accounts

Compare a template with existing accounts in a GnuCash file. Identifies
missing and extra accounts.

## Usage

``` r
compare_to_template(gc, template)
```

## Arguments

- gc:

  A GnuCashDB object

- template:

  Template list (from load_template) or template name

## Value

A list with missing and extra accounts
