% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getYF_functions.R
\name{getYH_qIS}
\alias{getYH_qIS}
\title{Returns Yahoo Finance Quarterly Income Statement of a set of companies (tickers)
To avoid heavy loads, by default, each query gets executed every second and the system stops 10 seconds after each 10 tickers.}
\usage{
getYH_qIS(tickers, block = 10, slp = 10)
}
\arguments{
\item{block}{'integer' After each block the query stops slp seconds. Default = 10}

\item{slp}{'integer' Number os seconds to sleep after each block. Default = 10}
}
\value{
list of tickers with the quarterly Income Statement
}
\description{
Returns Yahoo Finance Quarterly Income Statement of a set of companies (tickers)
To avoid heavy loads, by default, each query gets executed every second and the system stops 10 seconds after each 10 tickers.
}
\examples{
getYH_qIS(c("UNA.AS", "G.MI", "BMED.MI", "wrongticker","VOW3.DE"))
}
