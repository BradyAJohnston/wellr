# Read a `{plater}` Formatted Metadata File

Read a `{plater}` Formatted Metadata File

## Usage

``` r
plate_read_meta(file, sep = ",", plate = NULL)
```

## Arguments

- file:

  File path to a `{plater}` formatted `.csv` file which contains
  metadata information.

- sep:

  The separator for the `.csv` file, defaults to ",".

- plate:

  The size of the plate, such as 96 or 384, or its number of rows and
  columns. If `NULL`, the smallest standard plate that holds all the
  wells is used.

## Value

a tibble::tibble(). The `well` column holds
[`well()`](https://bradyajohnston.github.io/wellr/reference/well.md) IDs
that know the size of the plate.

## Examples

``` r

file_meta <- system.file(
  "extdata",
  "20220929_1steptimer20_metainfo.csv",
  package = "wellr"
)

plate_read_meta(file_meta)
#> # A tibble: 336 × 5
#>    well       strain  concentration promoter rbs  
#>    <well:384> <chr>           <dbl> <chr>    <chr>
#>  1 A02        pRW0041             0 PJ23100  wk2  
#>  2 A03        pRW0044             0 PJ23107  wk2  
#>  3 A04        pRW0046             0 PJ23109  wk2  
#>  4 A05        pRW0047             0 PJ23110  wk2  
#>  5 A06        pRW0048             0 PJ23111  wk2  
#>  6 A07        pRW0053             0 PJ23100  st8  
#>  7 A08        pRW0056             0 PJ23107  st8  
#>  8 A09        pRW0058             0 PJ23109  st8  
#>  9 A10        pRW0059             0 PJ23110  st8  
#> 10 A11        pRW0060             0 PJ23111  st8  
#> # ℹ 326 more rows
```
