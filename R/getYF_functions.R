#' Returns Yahoo Finance Profile of a set of companies (tickers)
#' Fields: city, country, sector, industry, website and fullTimeEmployees
#' To avoid heavy loads, by default, each query gets executed every second and the system stops 10 seconds after each 10 tickers.
#'
#' @references https://finance.yahoo.com/quote/VOW3.DE/profile?p=VOW3.DE
#' @param tickers
#' @param block 'integer' After each block the query stops slp seconds. Default = 10
#' @param slp 'integer' Number os seconds to sleep after each block. Default = 10
#'
#' @return list of tickers with the profile info
#' @export
#'
#' @examples getYFProfile(c("UNA.AS", "G.MI", "BMED.MI", "wrongticker","VOW3.DE"))
getYFProfile  <- function(tickers, block = 10, slp = 10) {
  # Set of cols to extract. Some tickers miss some of the fields.
  colsprofile <- c("city", "country", "sector", "industry", "website", "fullTimeEmployees")
  res <- queryYFquoteSummaryMany(tickers, colsprofile, module = "assetProfile", block, slp)
  res
}


#' Returns Yahoo Finance key Statistics of a set of companies (tickers)
##' To avoid heavy loads, by default, each query gets executed every second and the system stops 10 seconds after each 10 tickers.
#'
#' @references https://finance.yahoo.com/quote/VOW3.DE/key-statistics?p=VOW3.DE
#' @param tickers
#' @param block 'integer' After each block the query stops slp seconds. Default = 10
#' @param slp 'integer' Number os seconds to sleep after each block. Default = 10
#'
#' @return list of tickers with the Key Statistics info
#' @export
#' @examples getYFKeyStatistics(c("UNA.AS", "G.MI", "BMED.MI", "wrongticker","VOW3.DE"))
getYFKeyStatistics <- function(tickers, block = 10, slp = 10) {
  # Set of cols to extract. Some tickers miss some of the fields.
  colskeyStats <- c("enterpriseValue", "forwardPE", "profitMargins", "beta3Year", "bookValue", "priceToBook",
                    "totalAssets",  "yield", "priceToSalesTrailing12Months",
                    "earningsQuarterlyGrowth", "netIncomeToCommon",
                    "trailingEps", "enterpriseToRevenue", "enterpriseToEbitda",
                    "52WeekChange", "SandP52WeekChange")
  res <- queryYFquoteSummaryMany(tickers, colskeyStats, module = "defaultKeyStatistics", block, slp)
  res

}

#' Returns Yahoo Finance Basic Financials of a set of companies (tickers)
##' To avoid heavy loads, by default, each query gets executed every second and the system stops 10 seconds after each 10 tickers.
#'
#' @references https://finance.yahoo.com/quote/VOW3.DE/key-statistics?p=VOW3.DE
#' @param tickers
#' @param block 'integer' After each block the query stops slp seconds. Default = 10
#' @param slp 'integer' Number os seconds to sleep after each block. Default = 10
#'
#' @return list of tickers with the Basic Financials info
#' @export
#' @examples getYFFinancialsBasics(c("UNA.AS", "G.MI", "BMED.MI", "wrongticker","VOW3.DE"))
getYFFinancialsBasics <- function(tickers, block = 10, slp = 10) {
  # Set of cols to extract. Some tickers miss some of the fields.
  colskeyStats <- c("totalCash", "totalCashPerShare", "ebitda",              
                    "totalDebt",    "quickRatio",  "currentRatio", "totalRevenue", "debtToEquity" , "revenuePerShare" ,     
                    "returnOnAssets",  "returnOnEquity", "grossProfits",  "freeCashflow",  "operatingCashflow",  "earningsGrowth",        
                    "revenueGrowth", "grossMargins", "ebitdaMargins", "operatingMargins", "profitMargins", "financialCurrency"   )
  res <- queryYFquoteSummaryMany(tickers, colskeyStats, module = "financialData", block, slp)
  res
  
}


#' Returns Historical Prices in a data.frame
#' Fields: Date, volume, close, adjclose
#' 
#' @param tickers set of tickers
#'
#' @param range character Valid Ranges: 1y, 2y, 5y, 10y, ytd, max
#' @param periodicity character Valid Intervals: 1d, 1wk, 1mo
#' @param block 'integer' After each block the query stops slp seconds. Default = 10
#' @param slp 'integer' Number os seconds to sleep after each block. Default = 10
#'
#' @examples getYFHistPrices(c("UNA.AS", "G.MI", "BMED.MI", "wrongticker","VOW3.DE", "PUM.DE"), "1y", "1mo")
getYFHistPrices <- function(tickers, range, periodicity, block = 10, slp = 10) {
  cols2extract <- c("Date", "volume", "close", "adjclose")
  res <- lapply(seq_along(tickers), function(i) {
    message(paste("Ticker", i, tickers[i], sep =  " "))
    pause(i, block, slp)
    tmp <- queryYFchart(tickers[i], range, interval = periodicity)
    tmp <- tmp[ , colnames(tmp) %in% cols2extract]
  })
  names(res) <- tickers
  res
}
