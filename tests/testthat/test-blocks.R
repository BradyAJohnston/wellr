fl <- system.file(
  "extdata", "2024-02-29_vio_GFP_main.csv",
  package = "wellr"
)

test_that("test reading of lines", {
  expect_equal(length(.plate_read_lines(fl)), 1277)

})

test_that("reading of blocks", {
  lines <- readr::read_lines(fl)[seq(44) + 163]
  expect_snapshot(read_data_block(lines))
  expect_equal(dim(read_data_block(lines)), c(3936, 3))

  all_blocks <- get_all_blocks(.plate_read_lines(fl), include_id = TRUE, rownum = TRUE)
  expect_equal(length(all_blocks), 31)
  expect_equal(!purrr::is_empty(all_blocks), TRUE)
})

test_that("test detection of lines", {

  # takes some subset of lines for some quick testing of detecting the lines
  lines <- readr::read_lines(fl)[sequence(10) + 160]
  expect_equal(which(is_block_start(lines)), 5)
  expect_equal(which(is_data_lines(lines)), c(5:10))
  expect_equal(which(!is_data_lines(lines)), c(1:4))
  expect_equal(which(is_block_line(lines)), c(5:10))
  expect_equal(which(!is_block_line(lines)), c(1:4))
  lines <- stringr::str_remove(lines, "^,")
  expect_equal(which(is_block_start(lines)), 5)
  expect_equal(which(is_data_lines(lines)), c(5:10))
  expect_equal(which(!is_data_lines(lines)), c(1:4))
  expect_equal(which(is_block_line(lines)), c(5:10))
  expect_equal(which(!is_block_line(lines)), c(1:4))
})
