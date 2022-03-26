test_that("Well to Index: colwise = TRUE", {
  comparitors <- c(
    "A12" = 89,
    "B01" = 2,
    "C02" = 11,
    "H01" = 8
  )

  lapply(seq_along(comparitors), function(x) {
    testthat::expect_equal(
      well_to_index(names(comparitors[x]), colwise = TRUE),
      comparitors[[x]]
      )
  })
})
test_that("Well to Index: colwise = FALSE ", {

  comparitors <- c(
    "A12" = 12,
    "B01" = 13,
    "C02" = 26,
    "H01" = 85
  )

  lapply(seq_along(comparitors), function(x) {
    testthat::expect_equal(
      well_to_index(names(comparitors[x])),
      comparitors[[x]]
      )
  })
})
