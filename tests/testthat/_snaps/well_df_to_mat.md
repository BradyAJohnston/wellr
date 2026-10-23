# well_df_to_mat() errors informatively

    Code
      well_df_to_mat(rbind(plate, plate), "value")
    Condition
      Error in `well_df_to_mat()`:
      ! `df` must have one row per well.
      i "A01", "A02", and "A03" appear more than once.
    Code
      well_df_to_mat(plate, "value", plate = 24)
    Condition
      Error in `well_df_to_mat()`:
      ! "A07", "A08", and "A09" are outside a plate of 4 rows and 6 columns.

