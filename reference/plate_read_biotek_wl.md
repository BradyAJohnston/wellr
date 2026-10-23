# Read the `wavelength` data blocks from a *biotek* `.csv` file.

Read the `wavelength` data blocks from a *biotek* `.csv` file.

## Usage

``` r
plate_read_biotek_wl(file, format_well = TRUE, plate = NULL)
```

## Arguments

- file:

  File path to the `.csv` file.

- format_well:

  Whether to format the `well` column of the returned dataframes.

- plate:

  The size of the plate, such as 96 or 384, or its number of rows and
  columns. If `NULL`, the plate size recorded in the file is used, or
  else the smallest standard plate that holds all the wells.

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html).
The `well` column holds
[`well()`](https://bradyajohnston.github.io/wellr/reference/well.md) IDs
that know the size of the plate.

## Examples

``` r
file_data <- system.file(
  "extdata",
  "2024-02-29_vio_GFP_main.csv",
  package = "wellr"
)

plate_read_biotek_wl(file_data)
#> # A tibble: 23,616 × 4
#>    id    wavelength well      value
#>    <chr>      <dbl> <well:96> <dbl>
#>  1 1            300 A01       0.28 
#>  2 1            300 A02       0.284
#>  3 1            300 A03       0.285
#>  4 1            300 A04       0.285
#>  5 1            300 A05       0.285
#>  6 1            300 A06       0.284
#>  7 1            300 A07       0.275
#>  8 1            300 A08       0.279
#>  9 1            300 A09       0.282
#> 10 1            300 A10       0.28 
#> # ℹ 23,606 more rows
plate_read_biotek_wl(file_data, format = FALSE)
#> # A tibble: 23,616 × 4
#>    id    wavelength well      value
#>    <chr>      <dbl> <well:96> <dbl>
#>  1 1            300 A1        0.28 
#>  2 1            300 A2        0.284
#>  3 1            300 A3        0.285
#>  4 1            300 A4        0.285
#>  5 1            300 A5        0.285
#>  6 1            300 A6        0.284
#>  7 1            300 A7        0.275
#>  8 1            300 A8        0.279
#>  9 1            300 A9        0.282
#> 10 1            300 A10       0.28 
#> # ℹ 23,606 more rows
```
