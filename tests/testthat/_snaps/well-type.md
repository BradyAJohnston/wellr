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

