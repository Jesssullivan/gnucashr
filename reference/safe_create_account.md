# Safe Create Account

Create account with automatic backup.

## Usage

``` r
safe_create_account(gc, name, account_type, parent = NULL, ...)
```

## Arguments

- gc:

  GnuCashDB object (must be opened with read_only = FALSE)

- name:

  Account name

- account_type:

  Account type (BANK, CASH, ASSET, LIABILITY, etc.)

- parent:

  Parent account GUID or path (default: ROOT)

- ...:

  Additional arguments passed to create_account

## Value

Logged result with account GUID or error
