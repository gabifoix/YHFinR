context("queryYFquoteSummary")


test_that("queryYFquoteSummary returns something", {
  res <- queryYFquoteSummary("UNA.AS", "defaultKeyStatistics")
  testthat::expect_equal(nrow(res), 1)
})

test_that("queryYFquoteSummary does not crash with wrong tickers", {
  res <- queryYFquoteSummary("wrong.ticker", "defaultKeyStatistics")
  testthat::expect_null(res)
  testthat::expect_message(queryYFquoteSummary("wrong.ticker", "defaultKeyStatistics"), "Wrong ticker: wrong.ticker")
})
