# Rows and columns of a plate

`well_row()` gives the row letter of each well and `well_col()` its
column number, as factors whose levels are all the rows or all the
columns of the plate, in order. Unlike
[`well_to_row_let()`](https://bradyajohnston.github.io/wellr/reference/well_to_row_let.md)
and
[`well_to_col_num()`](https://bradyajohnston.github.io/wellr/reference/well_to_col_num.md),
they keep rows and columns with no wells in the data, so sorting,
counting, faceting and plotting show the whole plate.

## Usage

``` r
well_row(x, plate = NULL)

well_col(x, plate = NULL)
```

## Arguments

- x:

  Well IDs, such as `"A01"`.

- plate:

  The size of the plate, such as 96 or 384, or its number of rows and
  columns. If `NULL`, the plate of
  [`well()`](https://bradyajohnston.github.io/wellr/reference/well.md)
  IDs is used, or else the smallest standard plate that holds all the
  wells.

## Value

A factor the same length as `x`.

## Examples

``` r
x <- well(c("A01", "C05"), plate = 96)
well_row(x)
#> [1] A C
#> Levels: A B C D E F G H
well_col(x)
#> [1] 1 5
#> Levels: 1 2 3 4 5 6 7 8 9 10 11 12

# Every row of the plate is counted, even those with no wells
table(well_row(x))
#> 
#> A B C D E F G H 
#> 1 0 1 0 0 0 0 0 

well_row(c("A01", "C05"), plate = 384)
#> [1] A C
#> Levels: A B C D E F G H I J K L M N O P
```
