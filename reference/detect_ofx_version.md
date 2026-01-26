# Detect OFX Version

Determine whether an OFX file is version 1.x (SGML-based) or version 2.x
(XML-based).

## Usage

``` r
detect_ofx_version(path)
```

## Arguments

- path:

  Path to the OFX/QFX file

## Value

Character string: "1" for SGML, "2" for XML, "unknown" otherwise

## Examples

``` r
if (FALSE) { # \dontrun{
version <- detect_ofx_version("statement.ofx")
if (version == "1") {
  message("OFX 1.x SGML format detected")
}
} # }
```
