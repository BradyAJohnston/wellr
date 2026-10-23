# Logical test for well ID format.

Logical test for well ID format.

## Usage

``` r
is_well_id(x)
```

## Arguments

- x:

  A string vector.

## Value

A logical vector.

## Examples

``` r
is_well_id(c("a12", "a2", "a02", "foo1"))
#> [1]  TRUE  TRUE  TRUE FALSE
```
