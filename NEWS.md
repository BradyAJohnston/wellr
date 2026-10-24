# wellr 0.4.0

## New features

* Well IDs are now a vctrs type, created with `well()` or `as_well()`, that
  records the size of its plate. It is a character vector underneath, so
  values, printing, comparisons, `%in%`, joins and string functions are
  unchanged. It sorts in plate order and works as a discrete scale in ggplot2.
  Combining it with character, or with wells from a different plate, gives
  character.

* The readers (`plate_read_biotek()`, `plate_read_biotek_wl()`,
  `plate_read_tecan()` and `plate_read_meta()`) return the well type, using the
  plate size recorded in the file, a new `plate` argument, or else inferring it.
  `well_plate()`, `well_from_index()`, `well_mat_to_df()` and
  `well_mat_frames_to_df()` also return it, and `well_format()` keeps it.

* `well_to_index()` and `well_from_index()` take the plate size from the well
  IDs when `plate` isn't given.

* New `well_plate_size()` and `well_plate_dims()` give the plate of well IDs.
  48 well plates are now supported.

* New `well_row()` and `well_col()` give the rows and columns of well IDs as
  factors whose levels are all the rows or columns of the plate.

* `well_plot()` draws every well of the plate, with wells that have no data in
  grey, in every facet. It gains a `plate` argument.

## Bug fixes

* `well_df_to_mat()` uses `plate` when given, which was always overwritten.
  Wells missing from the data are `NA`, instead of the values shifting into the
  wrong wells and being recycled. Duplicated wells, and wells outside the
  plate, are now errors.

* `well_mat_to_df()` uses the shape of the matrix, instead of assuming a 96
  well plate and mislabelling the wells of other plates.

* `well_plot()` uses the `colour` argument for the well borders, which was
  ignored.

* `well_join()` is vectorised, and errors for rows outside 1 to 26 or strings
  that aren't rows, instead of giving IDs like "NA05".

* `well_from_index()` errors for indices outside the plate.

* Errors for invalid arguments now name the argument and say what's wrong.

* Fixed deprecation warnings from readr 2.2.0, dplyr 1.2.0 and tidyselect
  1.2.0.

## Minor improvements

* wellr now requires R 4.1.0, readr 2.0.0, ggplot2 3.4.0 and rlang 1.1.0, and
  no longer depends on scales.
