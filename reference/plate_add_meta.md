# Add `{plater{` Formatted Metadata to a Dataset

Add `{plater{` Formatted Metadata to a Dataset

## Usage

``` r
plate_add_meta(data, file)
```

## Arguments

- data:

  A tibble::tibble() which contains a 'well' column, that will have the
  metadata added to it.

- file:

  File patch to a `{plater}` formatted `.csv` file which contains the
  metadat information.

## Value

a tibble::tibble()

## Examples

``` r
file_data <- system.file(
  "extdata",
  "20220929_1steptimer20.csv",
  package = "wellr"
)
file_meta <- system.file(
  "extdata",
  "20220929_1steptimer20_metainfo.csv",
  package = "wellr"
)

plate_read_biotek(file_data) |>
  plate_add_meta(file_meta)
#> # A tibble: 19,200 × 8
#>     time well         lum od600 strain  concentration promoter rbs  
#>    <dbl> <well:384> <dbl> <dbl> <chr>           <dbl> <chr>    <chr>
#>  1  358. A01            3 0.09  NA                 NA NA       NA   
#>  2  358. A02           27 0.097 pRW0041             0 PJ23100  wk2  
#>  3  358. A03            4 0.091 pRW0044             0 PJ23107  wk2  
#>  4  358. A04            3 0.09  pRW0046             0 PJ23109  wk2  
#>  5  358. A05           32 0.096 pRW0047             0 PJ23110  wk2  
#>  6  358. A06            3 0.097 pRW0048             0 PJ23111  wk2  
#>  7  358. A07           78 0.094 pRW0053             0 PJ23100  st8  
#>  8  358. A08            2 0.095 pRW0056             0 PJ23107  st8  
#>  9  358. A09           20 0.095 pRW0058             0 PJ23109  st8  
#> 10  358. A10          103 0.093 pRW0059             0 PJ23110  st8  
#> # ℹ 19,190 more rows
```
