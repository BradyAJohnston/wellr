# Relative Distance Between Given Well and a Reference Row

Relative Distance Between Given Well and a Reference Row

## Usage

``` r
well_dis(row, col, ref_row, ref_col)
```

## Arguments

- row:

  Numeric vector of row numbers. Must be equal in length to col.

- col:

  Numeric vector of col numbers. Must be equal in length to row.

- ref_row:

  Single reference row number.

- ref_col:

  Single reference column number.

## Value

a numeric vector.

## Examples

``` r
well_dis(3, 4, ref_row = 5, ref_col = 5)
#> [1] 2.236068
well_dis(1:8, 1:8, ref_row = well_to_row_num("C4"), ref_col = well_to_col_num("C4"))
#> [1] 3.605551 2.236068 1.000000 1.000000 2.236068 3.605551 5.000000 6.403124
```
