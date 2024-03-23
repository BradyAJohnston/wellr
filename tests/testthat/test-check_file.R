test_that("File exists", {
  fl <- 'thisfile.csv'
  expect_error(plate_read_tecan(fl), regexp = stringr::fixed("doesn't exist!"))
})
