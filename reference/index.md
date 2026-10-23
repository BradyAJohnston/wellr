# Package index

## Plate Functions

Functions for working with plates

- [`plate_add_meta()`](https://bradyajohnston.github.io/wellr/reference/plate_add_meta.md)
  :

  Add `{plater{` Formatted Metadata to a Dataset

- [`plate_read_biotek()`](https://bradyajohnston.github.io/wellr/reference/plate_read_biotek.md)
  : Read Biotek Output CSV Files

- [`plate_read_biotek_wl()`](https://bradyajohnston.github.io/wellr/reference/plate_read_biotek_wl.md)
  :

  Read the `wavelength` data blocks from a *biotek* `.csv` file.

- [`plate_read_meta()`](https://bradyajohnston.github.io/wellr/reference/plate_read_meta.md)
  :

  Read a [plater](https://docs.ropensci.org/plater/) Formatted Metadata
  File

- [`plate_read_tecan()`](https://bradyajohnston.github.io/wellr/reference/plate_read_tecan.md)
  : Read the output of Tecan Plate Readers

## Well Functions

Functions for formatting and manipulating well-related properties.

- [`well()`](https://bradyajohnston.github.io/wellr/reference/well.md)
  [`as_well()`](https://bradyajohnston.github.io/wellr/reference/well.md)
  [`is_well()`](https://bradyajohnston.github.io/wellr/reference/well.md)
  [`well_plate_size()`](https://bradyajohnston.github.io/wellr/reference/well.md)
  [`well_plate_dims()`](https://bradyajohnston.github.io/wellr/reference/well.md)
  : Well IDs that know their plate
- [`well_check()`](https://bradyajohnston.github.io/wellr/reference/well_check.md)
  : Checks if a well ID contains a letter and a number in the correct
  format.
- [`well_df_to_mat()`](https://bradyajohnston.github.io/wellr/reference/well_df_to_mat.md)
  : Convert a data.frame to a Matrix in Plate Format
- [`well_df_to_mat_frames()`](https://bradyajohnston.github.io/wellr/reference/well_df_to_mat_frames.md)
  : Convert a DataFrame to a Multi-Frame Matrix
- [`well_dis()`](https://bradyajohnston.github.io/wellr/reference/well_dis.md)
  : Relative Distance Between Given Well and a Reference Row
- [`well_dis_plate()`](https://bradyajohnston.github.io/wellr/reference/well_dis_plate.md)
  : Relative Distance for an Entire Plate
- [`well_format()`](https://bradyajohnston.github.io/wellr/reference/well_format.md)
  : Format a Well to Uppercase and Padded Numbers
- [`well_from_index()`](https://bradyajohnston.github.io/wellr/reference/well_from_index.md)
  : Convert Numeric Index to Well ID.
- [`well_join()`](https://bradyajohnston.github.io/wellr/reference/well_join.md)
  : Join a row and column into a well ID.
- [`well_mat_frames_to_df()`](https://bradyajohnston.github.io/wellr/reference/well_mat_frames_to_df.md)
  : Turn a Multi-Frame Matrix into a DataFrame
- [`well_mat_to_df()`](https://bradyajohnston.github.io/wellr/reference/well_mat_to_df.md)
  : Convert a Plate Matrix to a DataFrame
- [`well_plate()`](https://bradyajohnston.github.io/wellr/reference/well_plate.md)
  : Create a Tibble of Plate Information
- [`well_plot()`](https://bradyajohnston.github.io/wellr/reference/well_plot.md)
  : Plot a Plate Map
- [`well_reorder_df()`](https://bradyajohnston.github.io/wellr/reference/well_reorder_df.md)
  : Reorder a Plate-Based DataFrame
- [`well_row()`](https://bradyajohnston.github.io/wellr/reference/well_row.md)
  [`well_col()`](https://bradyajohnston.github.io/wellr/reference/well_row.md)
  : Rows and columns of a plate
- [`well_to_col_num()`](https://bradyajohnston.github.io/wellr/reference/well_to_col_num.md)
  : Extract number from well ID.
- [`well_to_index()`](https://bradyajohnston.github.io/wellr/reference/well_to_index.md)
  : Converts Well ID to a Numeric Index
- [`well_to_row_let()`](https://bradyajohnston.github.io/wellr/reference/well_to_row_let.md)
  : Extracts letter from well ID.
- [`well_to_row_num()`](https://bradyajohnston.github.io/wellr/reference/well_to_row_num.md)
  : Convert Well ID to Row Number

## Reading Files

reading of different metadata and plate data files.

- [`read_meta()`](https://bradyajohnston.github.io/wellr/reference/read_meta.md)
  :

  Read a [plater](https://docs.ropensci.org/plater/) formatted metadata
  file

## Utilities

Utility tests for dealing with plate metadata.

- [`is_well_id()`](https://bradyajohnston.github.io/wellr/reference/is_well_id.md)
  : Logical test for well ID format.
- [`well()`](https://bradyajohnston.github.io/wellr/reference/well.md)
  [`as_well()`](https://bradyajohnston.github.io/wellr/reference/well.md)
  [`is_well()`](https://bradyajohnston.github.io/wellr/reference/well.md)
  [`well_plate_size()`](https://bradyajohnston.github.io/wellr/reference/well.md)
  [`well_plate_dims()`](https://bradyajohnston.github.io/wellr/reference/well.md)
  : Well IDs that know their plate
