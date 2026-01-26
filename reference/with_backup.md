# Automatic Backup Wrapper

Execute a function with automatic backup and rollback on error.

## Usage

``` r
with_backup(gc, fn, backup_dir = NULL)
```

## Arguments

- gc:

  GnuCashDB object

- fn:

  Function to execute (receives gc as argument)

- backup_dir:

  Backup directory

## Value

Result from fn or error with rollback info
