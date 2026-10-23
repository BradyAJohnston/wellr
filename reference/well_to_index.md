# Converts Well ID to a Numeric Index

Indexes along rows first, so A12 is index 12 and B01 is index 13 for a
96 well plate.

## Usage

``` r
well_to_index(x, plate = NULL, colwise = FALSE)
```

## Arguments

- x:

  string well ID

- plate:

  size of the plate. One of c(6, 12, 24, 48, 96, 384), or the number of
  rows and columns. If `NULL`, the plate of
  [`well()`](https://bradyajohnston.github.io/wellr/reference/well.md)
  IDs is used, or else a 96 well plate.

- colwise:

  if TRUE, index instead down the columns, so H01 is index 8, A12 is
  index 89 and B01 is index 2 for a 96 well plate.

## Value

numeric well index.

## Examples

``` r

# indexing along the row first
well_to_index(c("A10", "c3", "h12"))
#> [1] 10 27 96
well_to_index("H08")
#> [1] 92
well_to_index("h8")
#> [1] 92
well_to_index("C20")
#> [1] 44

# indexing instead down the column first
well_to_index(c("A10", "c3", "h12"), colwise = TRUE)
#> [1] 73 19 96
well_to_index("H08", colwise = TRUE)
#> [1] 64
well_to_index("h8", colwise = TRUE)
#> [1] 64
well_to_index("C20", colwise = TRUE)
#> [1] 155
```
