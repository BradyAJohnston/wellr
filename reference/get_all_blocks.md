# Read all data blocks from a file, with each dataframe being an entry in a list

Read all data blocks from a file, with each dataframe being an entry in
a list

## Usage

``` r
get_all_blocks(
  lines,
  temp = FALSE,
  format_well = TRUE,
  include_id = FALSE,
  second_wl = TRUE,
  rownum = FALSE
)
```

## Arguments

- lines:

  Vector of strings that are lines from a .csv file.

- temp:

  Whether to include the temp column of a file.

- format_well:

  Whether to format the well ID column of a file.

- include_id:

  Whether to include the label / ID information for the data block as a
  column in the resulting data frame.

- second_wl:

  Whether to include the second possible wavelength in column names when
  reading in.

- rownum:

  Whether to include row numbers from pre-pivoted blocks when reading
  in.
