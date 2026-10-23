# Extracts letter from well ID.

Extracts letter from well ID.

## Usage

``` r
well_to_row_let(x)
```

## Arguments

- x:

  Well ID as string.

## Value

a string which is the letter extracted from a well ID.

## Examples

``` r
well_to_row_let(c("A10", "c3", "h12"))
#> [1] "A" "C" "H"
well_to_row_let("H08")
#> [1] "H"
well_to_row_let("h8")
#> [1] "H"
well_to_row_let("C20")
#> [1] "C"
```
