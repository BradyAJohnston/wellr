test_that("reading tecan csv", {
  file <- system.file(
    'extdata', 'tecanON1.csv',
    package = 'wellr'
  )
  expect_snapshot(plate_read_tecan(file))
})

test_that("reading tecan xslsx", {
  file <- system.file(
    'extdata', 'tecanON1.xlsx',
    package = 'wellr'
  )
  expect_snapshot(plate_read_tecan(file))
})

test_that("can't read format") {
  file <- system.file(
    'extdata', 'tecanON1.csv.test',
    package = 'wellr'
  )
  expect_error(
    plate_read_tecan(file),
    "Unable to format of file"
  )
}
