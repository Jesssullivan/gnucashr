# Apply Template to GnuCash Database

Create accounts from a template in a GnuCash database. Uses write
operations to create the account structure.

## Usage

``` r
apply_template(gc, template, parent_guid = NULL, dry_run = FALSE)
```

## Arguments

- gc:

  A GnuCashDB object opened for writing

- template:

  Template list (from load_template) or template name

- parent_guid:

  Optional parent account GUID (default: root)

- dry_run:

  If TRUE, show what would be created without making changes

## Value

Invisible tibble of created accounts
