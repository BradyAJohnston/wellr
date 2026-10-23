# Format a Well to Uppercase and Padded Numbers

Format a Well to Uppercase and Padded Numbers

## Usage

``` r
well_format(x, num_width = 2, uppercase = TRUE)
```

## Arguments

- x:

  Vector of well IDs to be formatted.

- num_width:

  Width to pad out number component with 0's.

- uppercase:

  Logical, whether the letter should be upercase.

## Value

Vector of strings as formatted well IDs.
[`well()`](https://bradyajohnston.github.io/wellr/reference/well.md) IDs
keep their plate.

## Examples

``` r
well_format(c("A9", "c3", "h12"))
#> [1] "A09" "C03" "H12"
```
