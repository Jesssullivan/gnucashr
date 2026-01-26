# Create Account Hierarchy Diagram

Generate a text-based visualization of the account hierarchy.

## Usage

``` r
template_diagram(template, max_depth = NULL)
```

## Arguments

- template:

  Template list (from load_template) or template name

- max_depth:

  Maximum depth to display (NULL for all)

## Value

Character string with hierarchy diagram

## Examples

``` r
if (FALSE) { # \dontrun{
cat(template_diagram("small-business"))
} # }
```
