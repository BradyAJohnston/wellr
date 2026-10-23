# Convert a DataFrame to a Multi-Frame Matrix

Converts a DataFrame to a matrix where each row is a single time point,
and each column is a single well. The wells are in index order, indexing
across the rows so the first values are all from row A.

## Usage

``` r
well_df_to_mat_frames(data, value, frame, well)
```

## Arguments

- data:

  Data frame or tibble to convert to a multi-frame matrix.

- value:

  Column with value data that will be used to populate the matrix.

- frame:

  Column with the frame or time data that will be used to create the
  rows of the matrix.

- well:

  Column with the well IDs which will be used to create the columns of
  the matrix.

## Value

a matrix where each column is a well and each row is a time point.

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
#>            A01        A02        A03        A04         A05         A06
#> 1 -1.400043517 -0.5536994  0.4681544  0.9353632  0.07003485  0.86208648
#> 2  0.255317055  0.6289820  0.3629513  0.1764886 -0.63912332 -0.24323674
#> 3 -2.437263611  2.0650249 -1.3045435  0.2436855 -0.04996490 -0.20608719
#> 4 -0.005571287 -1.6309894  0.7377763  1.6235489 -0.25148344  0.01917759
#> 5  0.621552721  0.5124269  1.8885049  0.1120381  0.44479712  0.02956075
#> 6  1.148411606 -1.8630115 -0.0974451 -0.1339970  2.75541758  0.54982754
#>          A07        A08        A09         A10        A11        A12        B01
#> 1  1.0743459 -1.4707363  1.9243433  0.42418757 -0.3872136 -0.2088827 2.03936926
#> 2 -0.6650882  0.2841503  1.2983928  1.06310200 -0.7854327 -1.3994105 0.44945378
#> 3  1.1139524  1.3373204  0.7487913  1.04871262 -1.0567369  0.2585373 1.39181405
#> 4 -0.2458964  0.2366963  0.5562243 -0.03810289 -0.7955414 -0.4417995 0.42656655
#> 5 -1.1775633  1.3182934 -0.5482573  0.48614892 -1.7562754  0.5685999 0.10758399
#> 6 -0.9758506  0.5239098  1.1105349  1.67288261 -0.6905379  2.1268505 0.02229473
#>           B02
#> 1 -1.14619967
#> 2  0.84618466
#> 3  0.08171963
#> 4 -1.30511701
#> 5 -0.94491206
#> 6  0.45434159
```
