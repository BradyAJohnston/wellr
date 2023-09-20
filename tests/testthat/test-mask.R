test_that("masking lines", {

  file <- system.file("extdata", "plate_layout_96.csv", package = "wellr")
  lines <- readLines(file, warn = FALSE)

  expect_equal(length(lines), 30)
  expect_equal(which(.is_blank_line(lines)), c(10, 20, 21))
  expect_equal(length(.mask_blank_lines(lines)), 27)


  file <- system.file("extdata", "20220929_1steptimer20_metainfo.csv", package = "wellr")
  lines <- readLines(file, warn = FALSE)
  expect_equal(length(lines), 71)
  expect_equal(which(.is_blank_line(lines)), c(18, 36, 54))
  expect_equal(length(.mask_blank_lines(lines)), 68)

})


test_that("plate guessing", {
  sizes <- c(6, 24, 48, 96, 384, 1536)
  expect_equal(unlist(lapply(sizes - 1, .guess_plate_size)), sizes)

  plate_classifications <- as.numeric(as.character(cut(seq(1536), c(0, sizes), labels = sizes)))

  expect_equal(
    unlist(lapply(seq(1536), .guess_plate_size)),
    plate_classifications
  )
})
