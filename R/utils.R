
pause <- function(i, block = 10, slp = 10) {
  if (i/block == as.integer(i/block))
    Sys.sleep(slp)
}

tickerlog <- function (i, total, tickers) {
  message(paste("Ticker", i, "of", total, tickers[i], sep =  " "))
}

#' convert UNIX  to Date
#'
#' @param UNIXvalue
#'
#' @return Date
#' @example convertUNIX2date("1564351200")
convertUNIX2date <- function(UNIXvalue) {
  as.character(as.Date(as.POSIXct(as.numeric(UNIXvalue), origin="1970-01-01", tz = 'UTC')))
}


#' @example convertDate2UNIX("2020-08-31")
convertDate2UNIX <- function(date, format="%Y-%m-%d") {
  as.numeric(as.POSIXct(date, format))
}


#' @importFrom lubridate "%m-%"
#' @example calcInitDate("2020-08-31", range = "6y")
calcInitDate <- function(enddate, range) {
  if (grepl("y", range)) {
    nmonths <- as.numeric(gsub("[a-z]", "",range)) * 12
  } else {
    nmonths <- as.numeric(gsub("[a-z]", "",range))
  }
  as.Date(enddate) %m-% months(nmonths)
}

