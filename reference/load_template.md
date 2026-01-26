# Load an Account Template

Load an account template from the built-in templates or a custom file.

## Usage

``` r
load_template(template)
```

## Arguments

- template:

  Template name or path to JSON file

## Value

A nested list structure representing the chart of accounts

## Examples

``` r
if (FALSE) { # \dontrun{
template <- load_template("small-business")
template <- load_template("c-corporation")
} # }
```
