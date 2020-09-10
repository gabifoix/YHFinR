context("getYFIndexComp")


test_that("getYFIndexComp works as expected", {
  res <-  getYFIndexComp("^FCHI")
  testthat::expect_equal(nrow(res), 30)
  
})
