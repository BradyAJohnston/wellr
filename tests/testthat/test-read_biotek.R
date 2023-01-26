test_that("Test the Readinf of Biotek Files", {
  file_data <- system.file(
    "extdata",
    "20220929_1steptimer20.csv",
    package = "wellr"
  )

  file_meta <- system.file(
    "extdata",
    "20220929_1steptimer20_metainfo.csv",
    package = "wellr"
  )

  dat <- plate_read_biotek(file_data) %>%
    plate_add_meta(file_meta)

  expect_equal(
    well_format(
      dat$well[1:10],
      1,
      uppercase = FALSE
    ),
    paste0("a", 1:10)
  )
})
