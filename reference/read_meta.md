# Read a `{plater}` formatted metadata file

Read a `{plater}` formatted metadata file

## Usage

``` r
read_meta(file)
```

## Arguments

- file:

  to read metadata from.

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)

## Examples

``` r
file <- system.file("extdata", "plate_layout_96.csv", package = "wellr")
read_meta(file)
#> Warning: `read_meta()` was deprecated in wellr 0.3.2.
#> ℹ Please use `plate_read_meta()` instead.
#> ✖ This function will be removed in {wellr} v0.5.0
#> # A tibble: 75 × 4
#>    well          conc sample    protein
#>    <well:96>    <dbl> <chr>       <dbl>
#>  1 A01       1   e- 6 RNA    0.00000005
#>  2 A02       3.33e- 7 RNA    0.00000005
#>  3 A03       1.11e- 7 RNA    0.00000005
#>  4 A04       3.7 e- 8 RNA    0.00000005
#>  5 A05       1.23e- 8 RNA    0.00000005
#>  6 A06       4.12e- 9 RNA    0.00000005
#>  7 A07       1.37e- 9 RNA    0.00000005
#>  8 A08       4.57e-10 RNA    0.00000005
#>  9 A09       1.52e-10 RNA    0.00000005
#> 10 A10       5.08e-11 RNA    0.00000005
#> # ℹ 65 more rows
```
