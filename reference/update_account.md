# Update Account

Update an existing account's properties.

## Usage

``` r
update_account(
  gc,
  account_guid,
  name = NULL,
  description = NULL,
  code = NULL,
  hidden = NULL,
  placeholder = NULL
)
```

## Arguments

- gc:

  GnuCashDB object

- account_guid:

  Account GUID to update

- name:

  New name (optional)

- description:

  New description (optional)

- code:

  New code (optional)

- hidden:

  New hidden status (optional)

- placeholder:

  New placeholder status (optional)

## Value

Result with updated GUID or error
