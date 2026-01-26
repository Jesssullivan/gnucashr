# Batch Post Transactions

Post multiple transactions in a single database transaction.

## Usage

``` r
batch_post_transactions(gc, transactions)
```

## Arguments

- gc:

  GnuCashDB object

- transactions:

  List of transaction specifications, each with:

  - description: Transaction description

  - splits: List of splits

  - post_date: Optional date (default: today)

  - num: Optional reference

## Value

Result with list of GUIDs or error (all or nothing)
