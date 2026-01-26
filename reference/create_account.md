# Create a New Account

Create a new account in the GnuCash book.

## Usage

``` r
create_account(
  gc,
  name,
  account_type,
  parent = NULL,
  commodity_guid = NULL,
  description = NULL,
  code = NULL,
  placeholder = FALSE,
  hidden = FALSE
)
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

- commodity_guid:

  Currency GUID (default: book default currency)

- description:

  Optional account description

- code:

  Optional account code

- placeholder:

  Is this a placeholder account? (default: FALSE)

- hidden:

  Is this account hidden? (default: FALSE)

## Value

Result with new account GUID or error
