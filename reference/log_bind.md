# Bind Logged Operations (Monadic Bind)

Chain logged operations, accumulating logs.

## Usage

``` r
log_bind(x, fn)
```

## Arguments

- x:

  A logged object

- fn:

  Function that takes a value and returns a logged object

## Value

New logged object with combined logs
