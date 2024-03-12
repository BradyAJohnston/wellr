test_that("Reading WL from biotek .csv", {
  fl <- system.file("extdata", "2024-02-29_vio_GFP_main.csv", package = "wellr")
  dat <- plate_read_biotek_wl(fl)
  expect_snapshot(head(dat))
  expect_snapshot(tail(dat))
})

test_that("test detection of lines", {
  fl <- system.file("extdata", "2024-02-29_vio_GFP_main.csv", package = "wellr")

  # takes some subset of lines for some quick testing of detecting the lines
  lines <- readLines(fl)[sequence(10) + 160]
  expect_equal(which(is_block_start(lines)), 5)
  expect_equal(which(is_data_lines(lines)), c(5:10))
  expect_equal(which(!is_data_lines(lines)), c(1:4))
  expect_equal(which(is_block_line(lines)), c(5:10))
  expect_equal(which(!is_block_line(lines)), c(1:4))
})
