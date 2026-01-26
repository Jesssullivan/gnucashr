# Parse GnuCash XML File

Read and parse a GnuCash XML file into tibbles. Handles both compressed
(.gnucash) and uncompressed (.gnucash.xml) files.

## Usage

``` r
parse_gnucash_xml(path)
```

## Arguments

- path:

  Path to GnuCash XML file

## Value

List containing tibbles: accounts, transactions, splits, commodities,
prices
