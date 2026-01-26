# Safe Post Transaction

Post transaction with automatic backup and audit logging.

## Usage

``` r
safe_post_transaction(gc, description, splits, post_date = Sys.Date(), ...)
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

- ...:

  Additional arguments passed to post_transaction

## Value

Logged result with transaction GUID or error
