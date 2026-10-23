# Well IDs that know their plate

A `wellr_well` vector holds well IDs such as `"A01"`, along with the
size of the plate they come from. It is a character vector underneath,
so it prints, compares, joins and works with string functions exactly as
well IDs always have. Because it knows the plate size, functions such as
[`well_to_index()`](https://bradyajohnston.github.io/wellr/reference/well_to_index.md)
don't need to be told it.

- `well()` creates well IDs, formatting them as `"A01"`.

- `as_well()` converts a vector to well IDs, keeping their formatting.

- `is_well()` tests whether an object is a vector of well IDs.

- `well_plate_size()` and `well_plate_dims()` give the size of the
  plate, as the number of wells or as the number of rows and columns.

Combining well IDs from plates of the same size gives well IDs;
combining them with plain character vectors, or with well IDs from a
plate of a different size, gives a character vector. Use
[`as.character()`](https://rdrr.io/r/base/character.html) to get the
plain character vector.

## Usage

``` r
well(x = character(), plate = 96)

as_well(x, plate = NULL)

is_well(x)

well_plate_size(x)

well_plate_dims(x)
```

## Arguments

- x:

  For `well()` and `as_well()`, a character vector of well IDs. For the
  other functions, an object to test or a vector of well IDs.

- plate:

  The plate the wells come from: either the number of wells in a
  standard plate (6, 12, 24, 48, 96 or 384), or the number of rows and
  columns. If `NULL`, `as_well()` keeps the plate of a `wellr_well` and
  otherwise uses the smallest standard plate that contains all the
  wells.

## Value

`well()` and `as_well()` return a `wellr_well` vector. `is_well()`
returns a single logical. `well_plate_size()` returns the number of
wells and `well_plate_dims()` the number of rows and columns.

## Examples

``` r
x <- well(c("a1", "B2", "h12"), plate = 96)
x
#> <well<96>[3]>
#> [1] A01 B02 H12

x == "B02"
#> [1] FALSE  TRUE FALSE
well_to_index(x)
#> [1]  1 14 96
well_plate_size(x)
#> [1] 96

as_well(c("A01", "P24"))
#> <well<384>[2]>
#> [1] A01 P24
```
