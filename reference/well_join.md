# Join a row and column into a well ID.

Joins a column number and a row letter or number into a well ID. i.e.
joins "A" and "1" to "A01" and joins "3 and "10" to "C10".

## Usage

``` r
well_join(row, col, num_width = 2)
```

## Arguments

- row:

  either a row letter or number for to be coerced into a letter.

- col:

  a column number.

- num_width:

  the number of digits to pad out the column number with 0.

## Value

a vector of well IDs as strings.

## Examples

``` r

well_join(1:3, 4)
#> [1] "A04" "B04" "C04"
well_join(c("A", "B", "H"), 9)
#> [1] "A09" "B09" "H09"
well_join("C", 1:10)
#>  [1] "C01" "C02" "C03" "C04" "C05" "C06" "C07" "C08" "C09" "C10"
```
