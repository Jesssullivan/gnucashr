# Clean Old Backups

Remove backups older than specified age.

## Usage

``` r
clean_old_backups(gc, backup_dir = NULL, older_than_hours = 168)
```

## Arguments

- gc:

  GnuCashDB object or path

- backup_dir:

  Backup directory

- older_than_hours:

  Remove backups older than this (default: 168 = 1 week)

## Value

Result with count of removed backups
