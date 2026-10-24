# Changelog

## wellr 0.4.0

### New features

- Well IDs are now a vctrs type, created with
  [`well()`](https://bradyajohnston.github.io/wellr/reference/well.md)
  or
  [`as_well()`](https://bradyajohnston.github.io/wellr/reference/well.md),
  that records the size of its plate. It is a character vector
  underneath, so values, printing, comparisons, `%in%`, joins and string
  functions are unchanged. It sorts in plate order and works as a
  discrete scale in ggplot2. Combining it with character, or with wells
  from a different plate, gives character.

- The readers
  ([`plate_read_biotek()`](https://bradyajohnston.github.io/wellr/reference/plate_read_biotek.md),
  [`plate_read_biotek_wl()`](https://bradyajohnston.github.io/wellr/reference/plate_read_biotek_wl.md),
  [`plate_read_tecan()`](https://bradyajohnston.github.io/wellr/reference/plate_read_tecan.md)
  and
  [`plate_read_meta()`](https://bradyajohnston.github.io/wellr/reference/plate_read_meta.md))
  return the well type, using the plate size recorded in the file, a new
  `plate` argument, or else inferring it.
  [`well_plate()`](https://bradyajohnston.github.io/wellr/reference/well_plate.md),
  [`well_from_index()`](https://bradyajohnston.github.io/wellr/reference/well_from_index.md),
  [`well_mat_to_df()`](https://bradyajohnston.github.io/wellr/reference/well_mat_to_df.md)
  and
  [`well_mat_frames_to_df()`](https://bradyajohnston.github.io/wellr/reference/well_mat_frames_to_df.md)
  also return it, and
  [`well_format()`](https://bradyajohnston.github.io/wellr/reference/well_format.md)
  keeps it.

- [`well_to_index()`](https://bradyajohnston.github.io/wellr/reference/well_to_index.md)
  and
  [`well_from_index()`](https://bradyajohnston.github.io/wellr/reference/well_from_index.md)
  take the plate size from the well IDs when `plate` isn’t given.

- New
  [`well_plate_size()`](https://bradyajohnston.github.io/wellr/reference/well.md)
  and
  [`well_plate_dims()`](https://bradyajohnston.github.io/wellr/reference/well.md)
  give the plate of well IDs. 48 well plates are now supported.

- New
  [`well_row()`](https://bradyajohnston.github.io/wellr/reference/well_row.md)
  and
  [`well_col()`](https://bradyajohnston.github.io/wellr/reference/well_row.md)
  give the rows and columns of well IDs as factors whose levels are all
  the rows or columns of the plate.

- [`well_plot()`](https://bradyajohnston.github.io/wellr/reference/well_plot.md)
  draws every well of the plate, with wells that have no data in grey,
  in every facet. It gains a `plate` argument.

### Bug fixes

- [`well_df_to_mat()`](https://bradyajohnston.github.io/wellr/reference/well_df_to_mat.md)
  uses `plate` when given, which was always overwritten. Wells missing
  from the data are `NA`, instead of the values shifting into the wrong
  wells and being recycled. Duplicated wells, and wells outside the
  plate, are now errors.

- [`well_mat_to_df()`](https://bradyajohnston.github.io/wellr/reference/well_mat_to_df.md)
  uses the shape of the matrix, instead of assuming a 96 well plate and
  mislabelling the wells of other plates.

- [`well_plot()`](https://bradyajohnston.github.io/wellr/reference/well_plot.md)
  uses the `colour` argument for the well borders, which was ignored.

- [`well_join()`](https://bradyajohnston.github.io/wellr/reference/well_join.md)
  is vectorised, and errors for rows outside 1 to 26 or strings that
  aren’t rows, instead of giving IDs like “NA05”.

- [`well_from_index()`](https://bradyajohnston.github.io/wellr/reference/well_from_index.md)
  errors for indices outside the plate.

- Errors for invalid arguments now name the argument and say what’s
  wrong.

- Fixed deprecation warnings from readr 2.2.0, dplyr 1.2.0 and
  tidyselect 1.2.0.

### Minor improvements

- wellr now requires R 4.1.0, readr 2.0.0, ggplot2 3.4.0 and rlang
  1.1.0, and no longer depends on scales.
