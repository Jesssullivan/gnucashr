# Bind Result (Monadic Bind)

Chain result operations, short-circuiting on error.

## Usage

``` r
result_bind(x, fn)
```

## Arguments

- x:

  A result object

- fn:

  Function that takes a value and returns a result

## Value

Result from fn or the original error
