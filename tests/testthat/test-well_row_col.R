test_that("well_row() and well_col() have the plate's rows and columns as levels", {
  x <- well(c("C05", "A01"), plate = 96)

  expect_equal(well_row(x), factor(c("C", "A"), levels = LETTERS[1:8]))
  expect_equal(well_col(x), factor(c(5, 1), levels = 1:12))
  expect_equal(nlevels(well_row(c("A01", "B02"), plate = 384)), 16)
  expect_equal(nlevels(well_col(c("A01", "B02"), plate = 384)), 24)
})

test_that("well_row() infers the plate of plain well IDs", {
  expect_equal(levels(well_row(c("A01", "H12"))), LETTERS[1:8])
  expect_equal(levels(well_row(c("A01", "P24"))), LETTERS[1:16])
})

test_that("well_row() and well_col() error for wells outside the plate", {
  expect_snapshot(error = TRUE, {
    well_row("P24", plate = 96)
    well_col(1)
  })
})
