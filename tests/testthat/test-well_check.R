test_that("Checking the Well Checks", {
  expect_equal(wellr:::well_check("A05"), TRUE)
  expect_equal(wellr:::well_check("C20"), TRUE)
  expect_equal(wellr:::well_check("A20"), TRUE)
  expect_error(wellr:::well_check("20"))
  expect_error(wellr:::well_check("A"))
  expect_error(wellr:::well_check("CC20"))
  expect_error(wellr:::well_check("A-20"))
})
