# Extract number from well ID.

Extract number from well ID.

## Usage

``` r
well_to_col_num(x)
```

## Arguments

- x:

  Well ID as string in format "A10"

## Value

Numeric verctor of column numbers extracted from well ID.

## Examples

``` r
well_to_col_num(c("A10", "c3", "h12"))
#> [1] 10  3 12
well_to_col_num("H08")
#> [1] 8
well_to_col_num("h8")
#> [1] 8
```
