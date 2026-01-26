# Flatten Account Template to Data Frame

Convert a hierarchical account template to a flat data frame. Useful for
importing into GnuCash.

## Usage

``` r
flatten_template(template, include_guids = TRUE)
```

## Arguments

- template:

  Template list (from load_template) or template name

- include_guids:

  Generate GUIDs for each account

## Value

A tibble with account data

## Examples

``` r
if (FALSE) { # \dontrun{
accounts <- flatten_template("small-business")
} # }
```
