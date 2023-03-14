test_that("multiplication works", {
  dat <- plate_read_biotek(
    system.file("extdata", "20220929_1steptimer20.csv", package = "wellr")
  )

  mat <- well_df_to_mat_frames(dat, od600, time, well)
  expect_equal(dim(mat), c(50, 24))
})
