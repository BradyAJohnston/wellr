test_that("well_df_to_mat() places values by well", {
  plate <- well_plate()
  plate$value <- seq_len(96)

  expected <- matrix(as.numeric(1:96), nrow = 8, byrow = TRUE)
  expect_equal(well_df_to_mat(plate, "value"), expected)
  expect_equal(well_df_to_mat(plate[96:1, ], "value"), expected)
})

test_that("well_df_to_mat() fills missing wells with NA", {
  plate <- well_plate()
  plate$value <- seq_len(96)
  plate <- plate[plate$well != "B03", ]

  mat <- well_df_to_mat(plate, "value")
  expect_equal(mat[2, 3], NA_real_)
  expect_equal(mat[2, 4], 16)
  expect_equal(sum(is.na(mat)), 1)
})

test_that("well_df_to_mat() uses the plate argument", {
  plate <- tibble::tibble(well = c("A01", "B01", "G11"), value = 1:3)

  mat <- well_df_to_mat(plate, "value", plate = 384)
  expect_equal(dim(mat), c(16, 24))
  expect_equal(mat[c(1, 2, 7), 1], c(1, 2, NA))
  expect_equal(mat[7, 11], 3)
  expect_equal(dim(well_df_to_mat(plate, "value")), c(8, 12))
})

test_that("well_df_to_mat() uses the plate of well IDs", {
  plate <- tibble::tibble(
    well = well(c("A01", "B02"), plate = 384),
    value = 1:2
  )
  expect_equal(dim(well_df_to_mat(plate, "value")), c(16, 24))
})

test_that("well_df_to_mat() errors informatively", {
  plate <- well_plate()
  plate$value <- seq_len(96)

  expect_snapshot(error = TRUE, {
    well_df_to_mat(rbind(plate, plate), "value")
    well_df_to_mat(plate, "value", plate = 24)
  })
})
