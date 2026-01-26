# List Available Backups

List all available backups for a GnuCash file.

## Usage

``` r
list_backups(gc, backup_dir = NULL)
```

## Arguments

- gc:

  GnuCashDB object or path to GnuCash file

- backup_dir:

  Backup directory (default: auto-detect)

## Value

tibble of available backups
