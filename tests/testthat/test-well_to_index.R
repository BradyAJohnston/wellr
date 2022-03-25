test_that("multiplication works", {

  colwise_comparitors <- c(
    "A12" = 89,
    "B01" = 2,
    "C02" = 11,
    "H01" = 8
  )

  lapply(seq_along(colwise_comparitors), function(x) {
    testthat::expect_equal(
      well_to_index(names(colwise_comparitors[x]), colwise = TRUE),
      colwise_comparitors[[x]]
      )
  })

  rowwise_comparitors <- c(
    "A12" = 12,
    "B01" = 13,
    "C02" = 26,
    "H01" = 85
  )

  lapply(seq_along(rowwise_comparitors), function(x) {
    testthat::expect_equal(
      well_to_index(names(rowwise_comparitors[x])),
      rowwise_comparitors[[x]]
      )
  })
})
