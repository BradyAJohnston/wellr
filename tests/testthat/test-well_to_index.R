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

test_that("Well to Index: colwise = FALSE ", {
  comparitors <- c(
    "A12" = 12,
    "B01" = 13,
    "C02" = 26,
    "H01" = 85
  )

  lapply(seq_along(comparitors), function(x) {
    testthat::expect_equal(
      well_from_index(comparitors[x]),
      names(comparitors)[[x]]
    )
  })
})
test_that("Well to Index: colwise = TRUE ", {
  comparitors <- c(
    "A12" = 89,
    "B01" = 2,
    "C02" = 11,
    "H01" = 8
  )

  lapply(seq_along(comparitors), function(x) {
    testthat::expect_equal(
      well_from_index(comparitors[x], colwise = TRUE),
      names(comparitors)[[x]]
    )
  })
})

test_that("Well to Index: plate = 384, colwise = FALSE ", {
  comparitors <- c(
    "A12" = 12,
    "B01" = 25,
    "C02" = 50,
    "H01" = 169
  )

  lapply(seq_along(comparitors), function(x) {
    testthat::expect_equal(
      well_to_index(names(comparitors[x]), plate = 384),
      comparitors[[x]]
    )
  })
})
test_that("Well from Index: plate = 384, colwise = FALSE ", {
  comparitors <- c(
    "A12" = 12,
    "B01" = 25,
    "C02" = 50,
    "H01" = 169
  )

  lapply(seq_along(comparitors), function(x) {
    testthat::expect_equal(
      well_from_index(comparitors[x], plate = 384),
      names(comparitors)[[x]]
    )
  })
})
