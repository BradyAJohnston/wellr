# Turn a Multi-Frame Matrix into a DataFrame

Takes a matrix where each row is a time point and each column is a well
and converts it to a DataFrame containing a column for the well, a
column for the row, a column for the column, a column for the time and a
column for the values.

## Usage

``` r
well_mat_frames_to_df(mat, value = "value")
```

## Arguments

- mat:

  A matrix where each column is a well and each row is a frame from a
  multi time point experiment.

- value:

  Name for the column containing the values in the resulting data frame.

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)

## Examples

``` r
df_list <- lapply(1:10, function(x) {
  df <- wellr::well_plate()
  df$value <- rnorm(96)
  df$frame <- x
  df
})

df_frames <- do.call(rbind, df_list)

# convert the data frame to multi-frame matrix
mat <- well_df_to_mat_frames(
  data = df_frames,
  value = value,
  frame = frame,
  well = well
)

head(mat[, 1:14])
#>           A01        A02        A03        A04        A05        A06        A07
#> 1 -0.99245858 -1.0545187 -0.4161150  0.5625075 -0.3367150 -1.3768313 -2.1292973
#> 2 -0.02757795  0.6607691  0.9821496  0.6420557 -0.2158402  0.8831702  0.3376460
#> 3  2.22284516 -0.8234090  0.2421807 -0.8905326  0.6211323  0.6714203 -1.5745748
#> 4  0.15550390  0.4370337 -0.9379269 -0.1165375 -1.2840265  1.2300224 -0.1963194
#> 5 -0.86911632  0.3723881  1.5058585 -0.9419748 -1.3000924  1.6454679  0.9150601
#> 6 -1.17448945 -1.6467491 -1.0658512  1.1158279 -0.3767695 -0.1829260  1.0786365
#>          A08        A09        A10        A11        A12         B01
#> 1  1.0245716 -0.2805163  0.2661688  1.2124483  1.7631306 -0.99593086
#> 2 -0.6591855  0.5748366  0.5284032 -0.4453630  2.0258461  0.42011400
#> 3 -0.3863044 -0.2056768 -0.5793881  1.1386675  0.4276188 -0.06286163
#> 4 -0.2424989  0.2348805  0.9421994  0.1905235 -0.5664793 -0.08013185
#> 5  0.2205932  0.1106283 -0.7063614 -1.3663936 -0.8338353 -0.03228341
#> 6 -1.4810900  0.2742022 -1.2754021 -1.2981026  0.3982592 -0.71898093
#>           B02
#> 1 -2.03102701
#> 2  2.75076475
#> 3  1.51366972
#> 4  0.03398894
#> 5  0.74079278
#> 6 -1.27080818

# convert the matrix back to a dataframe
well_mat_frames_to_df(mat)
#> # A tibble: 960 × 5
#>    frame well       value   row   col
#>    <chr> <well:96>  <dbl> <dbl> <dbl>
#>  1 1     A01       -0.992     1     1
#>  2 1     A02       -1.05      1     2
#>  3 1     A03       -0.416     1     3
#>  4 1     A04        0.563     1     4
#>  5 1     A05       -0.337     1     5
#>  6 1     A06       -1.38      1     6
#>  7 1     A07       -2.13      1     7
#>  8 1     A08        1.02      1     8
#>  9 1     A09       -0.281     1     9
#> 10 1     A10        0.266     1    10
#> # ℹ 950 more rows
```
