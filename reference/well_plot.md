# Plot a Plate Map

Draws every well of the plate, coloured by `value`. Wells with no data
are drawn in grey, so the whole plate is shown even when only some wells
were used.

## Usage

``` r
well_plot(data, well, value, colour = "black", plate = NULL)
```

## Arguments

- data:

  Dataframe containing data.

- well:

  Name of the column with well IDs.

- value:

  Name of the column with the values to plot.

- colour:

  Colour of the well borders.

- plate:

  The size of the plate, such as 96 or 384, or its number of rows and
  columns. If `NULL`, the plate of
  [`well()`](https://bradyajohnston.github.io/wellr/reference/well.md)
  IDs is used, or else the smallest standard plate that holds all the
  wells.

## Value

ggplot2 object.

## Examples

``` r
dat <- wellr::well_plate()[, "well"]
dat$value <- rnorm(96)
well_plot(dat, well, value)


# Only some wells used: the rest of the plate is drawn empty
well_plot(dat[1:30, ], well, value)
```
