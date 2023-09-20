test_that("multiplication works", {
  file <- system.file("extdata", "plate_layout_96.csv", package = "wellr")
  meta <- read_meta(file)
  expect_equal(nrow(meta), 84)
})

test_that("read a meta plate", {
  fl <- system.file("extdata", "20220929_1steptimer20_metainfo.csv", package = "wellr")

  meta <- read_meta(fl)
  expect_equal(dim(meta), c(384, 5))
})

