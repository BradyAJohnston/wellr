# readers take the plate size as an argument

    Code
      plate_read_tecan(file, plate = 24)
    Condition
      Error in `plate_read_tecan()`:
      ! "A07", "A08", and "A09" are outside a plate of 4 rows and 6 columns.

# errors informatively

    Code
      well("foo")
    Condition
      Error in `well()`:
      ! "foo" is not a well ID.
    Code
      well("Q1", plate = 96)
    Condition
      Error in `well()`:
      ! "Q1" is outside a plate of 8 rows and 12 columns.
    Code
      well("A1", plate = 100)
    Condition
      Error in `well()`:
      ! `plate` must be a standard plate size or the number of rows and columns.
      i Standard plate sizes are "6", "12", "24", "48", "96", and "384".
    Code
      well_plate_size("A01")
    Condition
      Error in `well_plate_size()`:
      ! `x` must be well IDs from `well()`, not a string.
    Code
      well("A1") + 1
    Condition
      Error in `vec_arith()`:
      ! <well<96>> + <double> is not permitted
    Code
      abs(well("A1"))
    Condition
      Error in `vec_math()`:
      ! Can't use `abs()` on well IDs.

# well helpers check their arguments

    Code
      well_join(30, 1)
    Condition
      Error in `well_join()`:
      ! `row` must be row letters or whole numbers from 1 to 26.
      x Can't use 30.
    Code
      well_join("1x", 1)
    Condition
      Error in `well_join()`:
      ! `row` must be row letters or whole numbers from 1 to 26.
      x Can't use "1x".
    Code
      well_join(list(1), 1)
    Condition
      Error in `well_join()`:
      ! `row` must be a character vector or a numeric vector, not a list.
    Code
      well_join(1, 1, num_width = 0)
    Condition
      Error in `well_join()`:
      ! `num_width` must be a whole number larger than or equal to 1, not the number 0.
    Code
      well_to_index(1)
    Condition
      Error in `well_to_index()`:
      ! `x` must be a character vector, not the number 1.
    Code
      well_from_index("A01")
    Condition
      Error in `well_from_index()`:
      ! `x` must be a numeric vector, not the string "A01".
    Code
      well_from_index(97)
    Condition
      Error in `well_from_index()`:
      ! `x` must be whole numbers from 1 to 96, the wells of the plate.
      x Can't use 97.
    Code
      well_df_to_mat(plate, "value", well_col = "id")
    Condition
      Error in `well_df_to_mat()`:
      ! `well_col` must be a column of the data, not "id".
    Code
      well_df_to_mat(plate, "missing")
    Condition
      Error in `well_df_to_mat()`:
      ! `values_from` must be a column of the data, not "missing".

