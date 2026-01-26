# Import OFX/QFX File

Read and parse an OFX or QFX bank statement file. Uses Rcpp-accelerated
parsing for performance with large statement files.

## Usage

``` r
import_ofx(path, account_mapping = NULL)
```

## Arguments

- path:

  Path to the OFX/QFX file

- account_mapping:

  Optional named list mapping OFX transaction types to GnuCash account
  GUIDs. Keys are transaction types (DEBIT, CREDIT, CHECK, etc.), values
  are account GUIDs.

## Value

A tibble with columns:

- date:

  Transaction date (Date)

- description:

  Transaction description from NAME field (character)

- amount:

  Transaction amount, positive for credits (numeric)

- currency:

  Currency code from CURDEF (character)

- external_id:

  Bank's unique transaction ID - FITID (character)

- memo:

  Additional memo text (character)

- transaction_type:

  OFX transaction type: DEBIT, CREDIT, CHECK, etc. (character)

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic import
transactions <- import_ofx("bank_statement.ofx")

# With account mapping for import
mapping <- list(
  DEBIT = "expense_account_guid",
  CREDIT = "income_account_guid"
)
transactions <- import_ofx("statement.qfx", account_mapping = mapping)
} # }
```
