# Create Reactive Trial Balance

Creates a reactive trial balance that updates when the underlying
GnuCash data changes.

## Usage

``` r
reactive_trial_balance(gc_reactive, as_of_reactive = NULL)
```

## Arguments

- gc_reactive:

  Reactive GnuCashDB object

- as_of_reactive:

  Reactive date (optional)

## Value

A reactive expression returning a trial balance tibble

## Examples

``` r
if (FALSE) { # \dontrun{
gc <- reactive_gnucash(input$gnucash_file)
tb <- reactive_trial_balance(gc, reactive(input$as_of_date))
} # }
```
