# Create Account from Path

Create an account and any missing parent accounts from a path.

## Usage

``` r
create_account_path(gc, path, account_type, parent_type = NULL, ...)
```

## Arguments

- gc:

  GnuCashDB object

- path:

  Account path (e.g., "Assets:Current:Checking")

- account_type:

  Type for the leaf account

- parent_type:

  Type for auto-created parent accounts (default: same as leaf)

- ...:

  Additional arguments passed to create_account

## Value

Result with leaf account GUID or error
