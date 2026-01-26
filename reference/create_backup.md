# Create Backup of GnuCash File

Create a timestamped backup copy of a GnuCash file.

## Usage

``` r
create_backup(gc, backup_dir = NULL, max_backups = 10L)
```

## Arguments

- gc:

  GnuCashDB object or path to GnuCash file

- backup_dir:

  Directory for backups (default: same as source with .backup suffix)

- max_backups:

  Maximum number of backups to keep (default: 10)

## Value

Result with backup path or error
