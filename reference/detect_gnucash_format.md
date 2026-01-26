# Detect GnuCash File Format

Determine if a GnuCash file is SQLite or XML format.

## Usage

``` r
detect_gnucash_format(path)
```

## Arguments

- path:

  Path to GnuCash file

## Value

Character: "sqlite", "xml-gz", "xml", or "unknown"
