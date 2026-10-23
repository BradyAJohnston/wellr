# Create a Tibble of Plate Information

Generates a
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
that contains row, col, and well ID for the size of the plate specified
in nrow and ncol. If vectors of length `> 1` are supplied to either nrow
or ncol, the contents of the vectors are used instead of their numeric
value.

## Usage

``` r
well_plate(nrow = 8, ncol = 12)
```

## Arguments

- nrow:

  Number of rows to have in the generated plate.

- ncol:

  NUmber of columns to have in the generated plate.

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html).
The `well` column holds
[`well()`](https://bradyajohnston.github.io/wellr/reference/well.md) IDs
on a plate with this many rows and columns.

## Examples

``` r
well_plate(nrow = 8, ncol = 12)
#> # A tibble: 96 × 3
#>      row   col well     
#>    <int> <int> <well:96>
#>  1     1     1 A01      
#>  2     1     2 A02      
#>  3     1     3 A03      
#>  4     1     4 A04      
#>  5     1     5 A05      
#>  6     1     6 A06      
#>  7     1     7 A07      
#>  8     1     8 A08      
#>  9     1     9 A09      
#> 10     1    10 A10      
#> # ℹ 86 more rows
well_plate(nrow = 4, ncol = 6)
#> # A tibble: 24 × 3
#>      row   col well     
#>    <int> <int> <well:24>
#>  1     1     1 A01      
#>  2     1     2 A02      
#>  3     1     3 A03      
#>  4     1     4 A04      
#>  5     1     5 A05      
#>  6     1     6 A06      
#>  7     2     1 B01      
#>  8     2     2 B02      
#>  9     2     3 B03      
#> 10     2     4 B04      
#> # ℹ 14 more rows
```
