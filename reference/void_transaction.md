# Void Transaction

Mark a transaction as void (preserves history but zeroes amounts).

## Usage

``` r
void_transaction(gc, tx_guid, reason = "Voided")
```

## Arguments

- gc:

  GnuCashDB object

- tx_guid:

  Transaction GUID to void

- reason:

  Reason for voiding

## Value

Result with voided GUID or error
