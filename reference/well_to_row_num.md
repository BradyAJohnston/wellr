# Convert Well ID to Row Number

Calculates the row, but returns it as a number.

## Usage

``` r
well_to_row_num(x)
```

## Arguments

- x:

  Well ID as a string.

## Value

Numeric row number.

## Examples

``` r
well_to_row_num(c("A10", "c3", "h12"))
#> [1] 1 3 8
well_to_row_num("H08")
#> [1] 8
well_to_row_num("h8")
#> [1] 8
well_to_row_num("C20")
#> [1] 3
```
