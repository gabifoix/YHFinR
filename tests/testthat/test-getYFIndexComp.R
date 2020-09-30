context("getYFIndexComp")


test_that("getYFIndexComp works as expected", {
  res <-  getYFIndexComp("DAX")
  testthat::expect_equal(length(res), 30)
  
})
