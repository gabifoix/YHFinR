context("getYFIndexComp")

if (FALSE) {
  devtools::test(filter = "getYFIndexComp")
}

test_that("getYFIndexComp fetches DAX components", {
  res <-  getYFIndexComp("DAX")
  testthat::expect_equal(length(res), 30)
  testthat::expect_true(all(sapply(res, function(x) grepl(".DE$", x))))
  
})


test_that("getYFIndexComp fetches SP500 components", {
  res <-  getYFIndexComp("SP500")
  testthat::expect_gte(length(res), 500)
  testthat::expect_true(all(sapply(res, function(x) grepl("[A-Z]",x))))
})

test_that("getYFIndexComp fetches CAC components", {
  res <-  getYFIndexComp("CAC")
  testthat::expect_equal(length(res), 40)
})

test_that("getYFIndexComp fetches MIB components", {
  res <-  getYFIndexComp("MIB")
  testthat::expect_equal(length(res), 40)
  testthat::expect_true(all(sapply(res, function(x) grepl(".MI$", x))))
})



