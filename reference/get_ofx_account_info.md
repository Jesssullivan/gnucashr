# Get OFX Account Information

Extract account metadata from an OFX file, including bank ID, account
number, and statement date range.

## Usage

``` r
get_ofx_account_info(path)
```

## Arguments

- path:

  Path to the OFX/QFX file

## Value

List with:

- account_id:

  Account number (masked in display)

- bank_id:

  Bank routing number

- account_type:

  Account type (CHECKING, SAVINGS, etc.)

- org_name:

  Financial institution name

- date_start:

  Statement start date

- date_end:

  Statement end date

- ledger_balance:

  Ending balance if available

## Examples

``` r
if (FALSE) { # \dontrun{
info <- get_ofx_account_info("statement.ofx")
cat("Bank:", info$org_name, "\n")
cat("Account:", info$account_type, "\n")
cat("Period:", info$date_start, "to", info$date_end, "\n")
} # }
```
