context("queryYFchart")


test_that("queryYchart works with initial date", {
  res <- queryYFchart("SAN.MC", "1y", "1wk", from = "2020-08-31")
  testthat::expect_equal(head(res$Date, 1), "2019-09-01")
  testthat::expect_equal(tail(res$Date, 1), "2020-08-30")
  testthat::expect_false(any(is.na(res)))
})

test_that("queryYchart works without dates", {
  res <- queryYFchart("SAN.MC", "1y", "1wk")
  testthat::expect_gt(nrow(res), 50)
  testthat::expect_lt(nrow(res), 55)
  testthat::expect_false(any(is.na(res)))
})




