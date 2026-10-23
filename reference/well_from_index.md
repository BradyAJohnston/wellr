# Convert Numeric Index to Well ID.

Convert Numeric Index to Well ID.

## Usage

``` r
well_from_index(x, plate = NULL, num_width = 2, colwise = FALSE)
```

## Arguments

- x:

  numeric index to convert to a well ID

- plate:

  number of wells in the plate. One of c(6, 12, 24, 48, 96, 384), or the
  number of rows and columns. If `NULL`, a 96 well plate.

- num_width:

  number of zeros to pad the column number with to the left.

- colwise:

  if TRUE, index instead down the columns, so H01 is index 8, A12 is
  index 89 and B01 is index 2 for a 96 well plate.

## Value

[`well()`](https://bradyajohnston.github.io/wellr/reference/well.md) IDs
on the plate.

## Examples

``` r
# indexing first along the rows
well_from_index(1:20)
#> <well<96>[20]>
#>  [1] A01 A02 A03 A04 A05 A06 A07 A08 A09 A10 A11 A12 B01 B02 B03 B04 B05 B06 B07
#> [20] B08

# indexing first down the columns
well_from_index(1:20, colwise = TRUE)
#> <well<96>[20]>
#>  [1] A01 B01 C01 D01 E01 F01 G01 H01 A02 B02 C02 D02 E02 F02 G02 H02 A03 B03 C03
#> [20] D03
```
