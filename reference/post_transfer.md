# Post Simple Transfer

Convenience function for posting a simple two-account transfer.

## Usage

``` r
post_transfer(
  gc,
  from_account,
  to_account,
  amount,
  description,
  post_date = Sys.Date(),
  num = NULL
)
```

## Arguments

- gc:

  GnuCashDB object

- from_account:

  Source account (will be credited/reduced)

- to_account:

  Destination account (will be debited/increased)

- amount:

  Amount to transfer (positive number)

- description:

  Transaction description

- post_date:

  Transaction date (default: today)

- num:

  Optional reference number

## Value

Result with transaction GUID or error

## Examples

``` r
if (FALSE) { # \dontrun{
# Transfer $100 from checking to savings
post_transfer(gc, "Assets:Checking", "Assets:Savings", 100, "Monthly savings")

# Pay an expense
post_transfer(gc, "Assets:Checking", "Expenses:Utilities", 150, "Electric bill")
} # }
```
