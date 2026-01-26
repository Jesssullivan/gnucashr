# Detect OFX File Version

Determine whether an OFX file is version 1.x (SGML) or 2.x (XML).

## Usage

``` r
detect_ofx_version_cpp(content)
```

## Arguments

- content:

  String content of the OFX file

## Value

String: "1" for SGML, "2" for XML, "unknown" otherwise
