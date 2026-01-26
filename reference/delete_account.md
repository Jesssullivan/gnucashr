# Delete Account

Delete an account (must have no transactions and no children).

## Usage

``` r
delete_account(gc, account_guid, force = FALSE)
```

## Arguments

- gc:

  GnuCashDB object

- account_guid:

  Account GUID to delete

- force:

  Force delete even with children (will reparent children to
  grandparent)

## Value

Result with deleted GUID or error
