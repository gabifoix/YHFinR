% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getYF_functions.R
\name{getYFFreeCashFlow}
\alias{getYFFreeCashFlow}
\title{Get the Free Cash Flow (FCF) from Yahoo Finance of a set of companies (tickers)
The most recent data is taken.
FCF = Operating Cash Flow + Capital Expenditure (negative sign)}
\usage{
getYFFreeCashFlow(tickers, block = 10, slp = 10)
}
\arguments{
\item{block}{'integer' After each block the query stops slp seconds. Default = 10}

\item{slp}{'integer' Number os seconds to sleep after each block. Default = 10}
}
\value{
list of tickers with the Free Casf Flow
}
\description{
Get the Free Cash Flow (FCF) from Yahoo Finance of a set of companies (tickers)
The most recent data is taken.
FCF = Operating Cash Flow + Capital Expenditure (negative sign)
}
\examples{
getYFFreeCashFlow(c("UNA.AS", "AAPL", "BMED.MI", "wrongticker","VOW3.DE"))
}
\references{
https://finance.yahoo.com/quote/ITX.MC/cash-flow?p=ITX.MC
}
