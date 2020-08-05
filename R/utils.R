
pause <- function(i, block = 10, slp = 10) {
  if (i/block == as.integer(i/block))
    Sys.sleep(slp)
}



#' convert UNIX  to Date
#'
#' @param UNIXvalue
#'
#' @return Date
#' @examples convertUNIX2date("1564351200")
convertUNIX2date <- function(UNIXvalue) {
  as.character(as.Date(as.POSIXct(as.numeric(UNIXvalue), origin="1970-01-01", tz = 'UTC')))
}

