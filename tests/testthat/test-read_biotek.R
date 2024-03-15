test_that("Test the Reading of Biotek Files", {
    file = "20220929_1steptimer20.csv"
    meta = "20220929_1steptimer20_metainfo.csv"

    dat <- plate_read_biotek(system.file("extdata", file, package = "wellr"))
    dat <- plate_add_meta(
      data = dat,
      file = system.file("extdata", meta, package = "wellr")
    )

    expect_snapshot(head(dat))
})

test_that("Read multiple wavelengths", {
  file = "2024-02-29_vio_GFP_main.csv"

  dat <- plate_read_biotek(system.file("extdata", file, package = "wellr"))
  expect_snapshot(head(dat))
  dat <- plate_read_biotek(system.file("extdata", file, package = "wellr"), second_wl = TRUE)
  expect_snapshot(head(dat))

})
