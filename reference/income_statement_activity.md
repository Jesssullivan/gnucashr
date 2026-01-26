# Get Income Statement Activity

Extract income and expense activity for a specific period.

## Usage

``` r
income_statement_activity(gc, start_date, end_date = Sys.Date())
```

## Arguments

- gc:

  GnuCashDB object

- start_date:

  Start of period

- end_date:

  End of period

## Value

tibble with income/expense activity
