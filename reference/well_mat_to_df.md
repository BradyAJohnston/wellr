# Convert a Plate Matrix to a DataFrame

Takes a matrix that is formatted as a plate, and converts it to a
long-format 'tidy' data frame with a column for the well ID, the row,
the column and the values that were in the matrix.

## Usage

``` r
well_mat_to_df(matrix, value_col = "value")
```

## Arguments

- matrix:

  A matrix object to be converted.

- value_col:

  The name of the value column in the final DataFrame.

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)

## Examples

``` r
mat <- matrix(rnorm(96), ncol = 12)
well_mat_to_df(mat, "random_values")
#> # A tibble: 96 × 4
#>    well        row   col random_values
#>    <well:96> <dbl> <dbl>         <dbl>
#>  1 A01           1     1       -1.56  
#>  2 A02           1     2       -0.474 
#>  3 A03           1     3       -0.688 
#>  4 A04           1     4       -1.16  
#>  5 A05           1     5        0.0279
#>  6 A06           1     6        1.05  
#>  7 A07           1     7        0.270 
#>  8 A08           1     8        0.669 
#>  9 A09           1     9        0.943 
#> 10 A10           1    10        0.0103
#> # ℹ 86 more rows
```
