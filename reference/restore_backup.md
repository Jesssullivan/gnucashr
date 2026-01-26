# Restore from Backup

Restore a GnuCash file from a backup.

## Usage

``` r
restore_backup(backup_path, target_path = NULL, create_safety_backup = TRUE)
```

## Arguments

- backup_path:

  Path to backup file

- target_path:

  Target path (default: original location)

- create_safety_backup:

  Create backup of current file before restore (default: TRUE)

## Value

Result with restore info or error
