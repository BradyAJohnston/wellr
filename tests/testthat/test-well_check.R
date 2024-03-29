test_that("Checking the Well Checks", {
  expect_equal(wellr:::well_check("A05"), TRUE)
  expect_equal(wellr:::well_check("C20"), TRUE)
  expect_equal(wellr:::well_check("A20"), TRUE)
  expect_equal(wellr:::well_check("20"), FALSE)
  expect_equal(wellr:::well_check("A"), FALSE)
  expect_equal(wellr:::well_check("CC20"), FALSE)
  expect_equal(wellr:::well_check("NA600"), FALSE)
  expect_equal(wellr:::well_check("A-20"), FALSE)
  expect_equal(wellr:::well_check(c("A-20", "c3")), c(FALSE, TRUE))
})
