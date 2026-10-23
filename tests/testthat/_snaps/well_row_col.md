# well_row() and well_col() error for wells outside the plate

    Code
      well_row("P24", plate = 96)
    Condition
      Error in `well_row()`:
      ! "P24" is outside a plate of 8 rows and 12 columns.
    Code
      well_col(1)
    Condition
      Error in `well_col()`:
      ! `x` must be a character vector, not the number 1.

