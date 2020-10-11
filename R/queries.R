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
  total <- length(tickers)
  res <- lapply(seq_along(tickers), function(i) {
    tickerlog(i, total, tickers)
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
#' @export
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


#' Build basic Yahoo Finance URL to scrape
#'
#' @param ticker character
#' @param urlsection character section name of the URL
#'
#' @return character url
buildYahooFinanceURL <- function(ticker, urlsection) {
  generic <- paste0("https://finance.yahoo.com/quote/%s/", urlsection,"?p=%s")
  url <- sprintf(generic, ticker, ticker)
  url
}


#' Scrape Yahoo Finance
#' Returns the info of the web in different tables
#'
#' @param url 
#' @param time_in_seconds 
#'
#' @importFrom XML htmlTreeParse getNodeSet readHTMLTable
#' @importFrom dplyr bind_rows "%>%"
scrapeYahooFinance <- function(url, time_in_seconds = 1) {
  Sys.sleep(time_in_seconds)
  webpage <- readLines(url)
  html <- XML::htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
  tableNodes <- XML::getNodeSet(html, "//table")
  res <- lapply(tableNodes, function(x) XML::readHTMLTable(x)) %>% 
    dplyr::bind_rows() 
  Sys.sleep(time_in_seconds)
  res
}



#' Scrape Index Components from wikipedia
#' Example: https://en.wikipedia.org/wiki/List_of_S%26P_500_companies
#'
#' @param url wikipedia url
#' @param componentNode character Node where the table sits
#'
#' @return data.frame
#' @importFrom rvest html_nodes html_table
#' @importFrom xml2 read_html
#' @export
#'
#' @examples scrapeWikiIndexComponents("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies", "//*[@id="constituents"]")
scrapeWikiIndexComponents <- function(url, componentNode) {
  res <- url %>%
    read_html() %>%
    html_nodes(xpath = componentNode) %>% 
    html_table()
  res[[1]]
}


#' List of available indexes
#'
#'
#' @return
#' @export
available.indexes <- function(){
  source(system.file("indexes.list.R", package = "YHFinR"))
  names(indexes.list)
}


