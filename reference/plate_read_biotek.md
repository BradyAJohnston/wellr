# Read Biotek Output CSV Files

Read Biotek Output CSV Files

## Usage

``` r
plate_read_biotek(file, time_average = TRUE, second_wl = FALSE, plate = NULL)
```

## Arguments

- file:

  The filepath to a `.csv` file, exported from a Biotek plate reader.

- time_average:

  Logical, whether to average the time points across different
  observations (LUM / OD) and pivot them to their own columns.

- second_wl:

  Whether to include the second wavelength when reading in data blocks,
  or to keep just the label and the first wavelength.

- plate:

  The size of the plate, such as 96 or 384, or its number of rows and
  columns. If `NULL`, the plate size recorded in the file is used, or
  else the smallest standard plate that holds all the wells.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html).
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

plate_read_biotek(file_data)
#> # A tibble: 14,304 × 7
#>     time well      od600 gfp_485 vio_575 vio_585 vio_595
#>    <dbl> <well:96> <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1  469. A01       0.041     260   0.042   0.041   0.041
#>  2  469. A02       0.043     258   0.044   0.044   0.043
#>  3  469. A03       0.043     258   0.044   0.044   0.044
#>  4  469. A04       0.044     254   0.044   0.044   0.044
#>  5  469. A05       0.044     256   0.044   0.044   0.044
#>  6  469. A06       0.043     255   0.044   0.044   0.044
#>  7  469. A07       0.043     244   0.044   0.044   0.044
#>  8  469. A08       0.045     252   0.045   0.045   0.045
#>  9  469. A09       0.045     251   0.045   0.045   0.045
#> 10  469. A10       0.045     247   0.045   0.046   0.045
#> # ℹ 14,294 more rows
plate_read_biotek(file_data, second_wl = TRUE)
#> # A tibble: 14,304 × 7
#>     time well      od600 gfp_485_530 vio_575 vio_585 vio_595
#>    <dbl> <well:96> <dbl>       <dbl>   <dbl>   <dbl>   <dbl>
#>  1  469. A01       0.041         260   0.042   0.041   0.041
#>  2  469. A02       0.043         258   0.044   0.044   0.043
#>  3  469. A03       0.043         258   0.044   0.044   0.044
#>  4  469. A04       0.044         254   0.044   0.044   0.044
#>  5  469. A05       0.044         256   0.044   0.044   0.044
#>  6  469. A06       0.043         255   0.044   0.044   0.044
#>  7  469. A07       0.043         244   0.044   0.044   0.044
#>  8  469. A08       0.045         252   0.045   0.045   0.045
#>  9  469. A09       0.045         251   0.045   0.045   0.045
#> 10  469. A10       0.045         247   0.045   0.046   0.045
#> # ℹ 14,294 more rows
```
