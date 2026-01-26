# Post a Transaction

Create a new transaction with splits in GnuCash.

## Usage

``` r
post_transaction(
  gc,
  description,
  splits,
  post_date = Sys.Date(),
  num = NULL,
  currency = NULL
)
```

## Arguments

- gc:

  GnuCashDB object (must be opened with read_only = FALSE)

- description:

  Transaction description

- splits:

  List of splits, each containing:

  - account: Account GUID or path

  - value: Numeric value (positive = debit, negative = credit)

  - memo: Optional memo

  - action: Optional action

- post_date:

  Transaction date (default: today)

- num:

  Optional transaction number/reference

- currency:

  Currency mnemonic (default: book default)

## Value

Result with new transaction GUID or error

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple two-split transaction
post_transaction(gc, "Office supplies",
  splits = list(
    list(account = "Expenses:Office", value = 50.00),
    list(account = "Assets:Checking", value = -50.00)
  )
)

# Multi-split transaction
post_transaction(gc, "Payroll",
  splits = list(
    list(account = "Expenses:Salary", value = 5000),
    list(account = "Expenses:Payroll Tax", value = 382.50),
    list(account = "Liabilities:Payroll Tax", value = -382.50),
    list(account = "Assets:Checking", value = -5000)
  )
)
} # }
```
