test_that("read a meta plate", {
  fl <- system.file("extdata", "20220929_1steptimer20_metainfo.csv", package = "wellr")
  meta <- read_meta(fl)

  expect_equal(dim(meta), c(384, 5))
  expect_equal(dim(read_meta(rep(fl, 3))), c(384 * 3, 6))
})
