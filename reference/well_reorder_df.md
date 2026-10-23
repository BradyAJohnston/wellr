# Reorder a Plate-Based DataFrame

Reorder a Plate-Based DataFrame

## Usage

``` r
well_reorder_df(data, well_col = "well", row_col = NULL, col_col = NULL)
```

## Arguments

- data:

  Data frame to reorder, than contains a row column and a col column.

- well_col:

  Column containing the well IDs.

- row_col:

  Column containing the row letters or numbers.

- col_col:

  Column containing the column numbers.

## Value

A data.frame that has been reordered according to the `row` & `col`
columns.

## Examples

``` r
df <- well_plate(nrow = 8, ncol = 12)
df <- df[sample(1:96, 96), ]
head(df)
#> # A tibble: 6 × 3
#>     row   col well     
#>   <int> <int> <well:96>
#> 1     2     3 B03      
#> 2     1     4 A04      
#> 3     6     5 F05      
#> 4     2     4 B04      
#> 5     6    10 F10      
#> 6     6     8 F08      

df <- well_reorder_df(df)
#> Warning: argument is not an atomic vector; coercing
#> Warning: argument is not an atomic vector; coercing
head(df)
#> # A tibble: 6 × 3
#>     row   col well     
#>   <dbl> <dbl> <well:96>
#> 1    NA    NA B03      
#> 2    NA    NA A04      
#> 3    NA    NA F05      
#> 4    NA    NA B04      
#> 5    NA    NA F10      
#> 6    NA    NA F08      
```
