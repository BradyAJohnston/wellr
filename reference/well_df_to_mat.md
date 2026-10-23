# Convert a data.frame to a Matrix in Plate Format

Extracts row and column information from the column specified by
`well_col` (defaults to 'well') and uses this to create a matrix that
contains values from the `values_from` column. Wells missing from `df`
are `NA`.

## Usage

``` r
well_df_to_mat(df, values_from, well_col = "well", plate = NULL)
```

## Arguments

- df:

  A dataframe containing at least a column called "well" that contains
  well IDs, with one row per well.

- values_from:

  Column that contains values to populate matrix.

- well_col:

  Column containing the well IDs to extract row and column information
  from.

- plate:

  Size of the plate, such as 96 or 384, or its number of rows and
  columns. If `NULL`, the plate of
  [`well()`](https://bradyajohnston.github.io/wellr/reference/well.md)
  IDs is used, or else the smallest standard plate that holds all the
  wells.

## Value

A matrix with a row for each row of the plate and a column for each
column, containing the values that were present in the data frame in the
`values_from` column.

## Examples

``` r
library(wellr)

# pivot a long data frame into a matrix based on the row and col columns,
# with values from a given column
plate <- well_plate(8, 12)
plate$value <- seq(nrow(plate))
well_df_to_mat(plate, values_from = "value")
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
#> [1,]    1    2    3    4    5    6    7    8    9    10    11    12
#> [2,]   13   14   15   16   17   18   19   20   21    22    23    24
#> [3,]   25   26   27   28   29   30   31   32   33    34    35    36
#> [4,]   37   38   39   40   41   42   43   44   45    46    47    48
#> [5,]   49   50   51   52   53   54   55   56   57    58    59    60
#> [6,]   61   62   63   64   65   66   67   68   69    70    71    72
#> [7,]   73   74   75   76   77   78   79   80   81    82    83    84
#> [8,]   85   86   87   88   89   90   91   92   93    94    95    96

# the function also handles missing values from a plate, filling with with NA
# if certain wells are missing
plate <- well_plate(7, 11)
plate$value <- seq(nrow(plate))
well_df_to_mat(plate, values_from = "value", plate = 384)
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
#>  [1,]    1    2    3    4    5    6    7    8    9    10    11    NA    NA
#>  [2,]   12   13   14   15   16   17   18   19   20    21    22    NA    NA
#>  [3,]   23   24   25   26   27   28   29   30   31    32    33    NA    NA
#>  [4,]   34   35   36   37   38   39   40   41   42    43    44    NA    NA
#>  [5,]   45   46   47   48   49   50   51   52   53    54    55    NA    NA
#>  [6,]   56   57   58   59   60   61   62   63   64    65    66    NA    NA
#>  [7,]   67   68   69   70   71   72   73   74   75    76    77    NA    NA
#>  [8,]   NA   NA   NA   NA   NA   NA   NA   NA   NA    NA    NA    NA    NA
#>  [9,]   NA   NA   NA   NA   NA   NA   NA   NA   NA    NA    NA    NA    NA
#> [10,]   NA   NA   NA   NA   NA   NA   NA   NA   NA    NA    NA    NA    NA
#> [11,]   NA   NA   NA   NA   NA   NA   NA   NA   NA    NA    NA    NA    NA
#> [12,]   NA   NA   NA   NA   NA   NA   NA   NA   NA    NA    NA    NA    NA
#> [13,]   NA   NA   NA   NA   NA   NA   NA   NA   NA    NA    NA    NA    NA
#> [14,]   NA   NA   NA   NA   NA   NA   NA   NA   NA    NA    NA    NA    NA
#> [15,]   NA   NA   NA   NA   NA   NA   NA   NA   NA    NA    NA    NA    NA
#> [16,]   NA   NA   NA   NA   NA   NA   NA   NA   NA    NA    NA    NA    NA
#>       [,14] [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24]
#>  [1,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#>  [2,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#>  [3,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#>  [4,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#>  [5,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#>  [6,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#>  [7,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#>  [8,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#>  [9,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#> [10,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#> [11,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#> [12,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#> [13,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#> [14,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#> [15,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
#> [16,]    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
```
