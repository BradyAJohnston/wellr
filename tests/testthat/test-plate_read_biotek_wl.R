test_that("Reading WL from biotek .csv", {
  fl <- system.file('extdata', '2024-02-29_vio_GFP_main.csv', package = "wellr")
  dat <- plate_read_biotek_wl(fl)
  expect_snapshot(head(dat))
  expect_snapshot(tail(dat))
})
