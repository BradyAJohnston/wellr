# Read the output of Tecan Plate Readers

Read the output of Tecan Plate Readers

## Usage

``` r
plate_read_tecan(file, temp = FALSE, plate = NULL)
```

## Arguments

- file:

  File path to the `.xlsx` or `.csv` file.

- temp:

  Logical, whether to include the temperature column.

- plate:

  The size of the plate, such as 96 or 384, or its number of rows and
  columns. If `NULL`, the plate size recorded in the file is used, or
  else the smallest standard plate that holds all the wells.

## Value

a
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
of the values from the file. The `well` column holds
[`well()`](https://bradyajohnston.github.io/wellr/reference/well.md) IDs
that know the size of the plate.

## Examples

``` r

# read a .csv tecan file
fl <- system.file(
  'extdata', 'tecanON1.csv',
  package = 'wellr'
)

plate_read_tecan(fl)
#> # A tibble: 11,520 × 5
#>    cycle_nr time_s well       od600  lumi
#>       <dbl>  <dbl> <well:96>  <dbl> <dbl>
#>  1        1      0 A01       0.0459    -4
#>  2        1      0 A02       0.0455    -2
#>  3        1      0 A03       0.0456    -5
#>  4        1      0 A04       0.0448     3
#>  5        1      0 A05       0.0444   110
#>  6        1      0 A06       0.0452     6
#>  7        1      0 A07       0.0442   127
#>  8        1      0 A08       0.0454    -2
#>  9        1      0 A09       0.0458    -2
#> 10        1      0 A10       0.046     -3
#> # ℹ 11,510 more rows

# read a .xlsx tecan file
fl <- system.file(
  'extdata', 'tecanON1.xlsx',
  package = 'wellr'
)

plate_read_tecan(fl)
#> # A tibble: 11,520 × 5
#>    cycle_nr time_s well       od600  lumi
#>       <dbl>  <dbl> <well:96>  <dbl> <dbl>
#>  1        1      0 A01       0.0459    -4
#>  2        1      0 A02       0.0455    -2
#>  3        1      0 A03       0.0456    -5
#>  4        1      0 A04       0.0448     3
#>  5        1      0 A05       0.0444   110
#>  6        1      0 A06       0.0452     6
#>  7        1      0 A07       0.0442   127
#>  8        1      0 A08       0.0454    -2
#>  9        1      0 A09       0.0458    -2
#> 10        1      0 A10       0.0460    -3
#> # ℹ 11,510 more rows
```
