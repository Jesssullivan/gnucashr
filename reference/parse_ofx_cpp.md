# Parse OFX Content

Parse OFX/QFX file content and extract transaction data. Handles both
OFX 1.x (SGML) and 2.x (XML) formats.

## Usage

``` r
parse_ofx_cpp(content)
```

## Arguments

- content:

  String content of the OFX file

## Value

List with vectors: dates, amounts, names, fitids, memos, trntype,
currency
