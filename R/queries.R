#' Query Yahoo Finance quoteSummary
#'
#' Queries Financial information from Yahoo Finance for a single ticker and a single module.
#' Modules <- c(assetProfile', 'defaultKeyStatistics', 'financialData',
#' 'industryTrend', 'indexTrend', 'sectorTrend',
#' 'incomeStatementHistory', 'incomeStatementHistoryQuarterly',
#' 'balanceSheetHistory', 'balanceSheetHistoryQuarterly',
#' 'cashflowStatementHistory','cashflowStatementHistoryQuarterly',
#' 'earnings', 'earningsHistory', 'earningsTrend',
#' 'calendarEvents', 'secFilings', 'recommendationTrend', 'upgradeDowngradeHistory',
#' 'institutionOwnership', 'fundOwnership', 'majorDirectHolders', 'majorHoldersBreakdown',
#' 'insiderTransactions', 'insiderHolders', 'netSharePurchaseActivity')
#'
#' @importFrom jsonlite fromJSON
#' @importFrom curl curl
#' @examples queryYFquoteSummary("UNA.AS", "defaultKeyStatistics")
queryYFquoteSummary <- function(ticker, module, time_in_seconds = 0.5) {
  urlroot <- "https://query1.finance.yahoo.com"
  quoteSummary <- sprintf("/v10/finance/quoteSummary/%s?modules=", ticker)
  url <- paste0(urlroot, quoteSummary, module)
  Sys.sleep(time_in_seconds)
  tmp <- try(jsonlite::fromJSON(curl::curl(url)), silent = TRUE)
  if (inherits(tmp, "try-error")) {
    message(paste0("Wrong ticker: ", ticker))
  } else {
    tmp$quoteSummary$result[[module]]
  }
}


#' Wrapper around queryYFquoteSummary
#' Pre-defined the fields to extract
#'
#' @param tickers 
#' @param cols2extract 
#' @param module 
#' @param block 
#' @param slp 
#'
#' @return list of tickers
#' @examples
queryYFquoteSummaryMany <- function(tickers, cols2extract, module, block = 10, slp = 10) {
  res <- lapply(seq_along(tickers), function(i) {
    message(paste("Ticker", i, tickers[i], sep =  " "))
    pause(i, block, slp)
    tmp <- queryYFquoteSummary(tickers[i], module)
    tmp <- tmp[ , colnames(tmp) %in% cols2extract]
  })
  names(res) <- tickers
  res
}



#' Query Yahoo Finance chart for one single ticker.
#' "chart" is the section where the historical prices are stored.
#' Returns a data.frame with "Date" column as character and "volume", "close" and "adjclose" as numeric.
#'
#' @references:
#' https://github.com/dennislwy/YahooFinanceAPI
#' https://www.stock-data-solutions.com/kb/how-to-load-historical-prices-from-yahoo-finance-to-excel.htm
#'
#' @param ticker
#' @param range character Valid Ranges: 1d, 5d, 1mo, 3mo, 6mo, 1y, 2y, 5y, 10y, ytd, max
#' @param interval character Valid Intervals: 1d, 1wk, 1mo
#' @param from Date "YYYY-MM-DD" from which the range counts back. Default Sys.Date
#' @param time_in_seconds
#'
#' @importFrom jsonlite fromJSON
#' @importFrom curl curl
#' @examples queryYFchart("SAN.MC", "1y", "1wk", from = "2020-08-30")
queryYFchart <- function(ticker, range, interval, from = NULL, time_in_seconds = 0.5) {
  urlroot <- "https://query1.finance.yahoo.com"
  if (missing(from)) {
    quoteSummary <- sprintf("/v7/finance/chart/%s?range=%s&interval=%s&indicators=quote&includeTimestamps=true", ticker, range, interval)
  } else {
    enddate <- convertDate2UNIX(from)
    initdate <- convertDate2UNIX(calcInitDate(from, range))
    quoteSummary <- sprintf("/v7/finance/chart/%s?period1=%s&period2=%s&interval=%s&indicators=quote&includeTimestamps=true", ticker, initdate, enddate, interval)
  }
  url <- paste0(urlroot, quoteSummary)
  Sys.sleep(time_in_seconds)
  tmp <- try(jsonlite::fromJSON(curl::curl(url)), silent = TRUE)
  if (inherits(tmp, "try-error")) {
    message(paste0("Wrong ticker: ", ticker))
  } else {
    # Build data.frame
    tmp <- data.frame(Date = sapply(tmp$chart$result$timestamp[[1]], convertUNIX2date),
                     volume = tmp$chart$result$indicators$quote[[1]]$volume[[1]],
                     close = tmp$chart$result$indicators$quote[[1]]$close[[1]],
                     adjclose = tmp$chart$result$indicators$adjclose[[1]]$adjclose[[1]],
                     stringsAsFactors = FALSE)
  }
  tmp
}

# https://query1.finance.yahoo.com/v7/finance/chart/SAN.MC?period1=1492524105&period2=1495116105&interval=1wk&indicators=quote&includeTimestamps=true
#https://query1.finance.yahoo.com/v7/finance/download/AAPL?period1=1492524105&period2=1495116105&interval=1d&events=history&crumb=tO1hNZoUQeQ

