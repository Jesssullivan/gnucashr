# Create Reactive GnuCash Connection

Creates a reactive GnuCash database connection that can be used in Shiny
or Quarto interactive documents.

## Usage

``` r
reactive_gnucash(path, refresh_interval = 60000)
```

## Arguments

- path:

  Path to GnuCash file (reactive input)

- refresh_interval:

  Refresh interval in milliseconds (default: 60000)

## Value

A reactive expression returning a GnuCashDB object

## Examples

``` r
if (FALSE) { # \dontrun{
# In a Quarto dashboard
gc <- reactive_gnucash(input$gnucash_file)
} # }
```
