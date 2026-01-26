# Pattern Match on Result

Handle both Ok and Err cases with separate functions.

## Usage

``` r
result_match(x, ok_fn, err_fn)
```

## Arguments

- x:

  A result object

- ok_fn:

  Function to call if Ok

- err_fn:

  Function to call if Err

## Value

Result of the matched function
