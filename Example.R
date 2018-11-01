#### Working example ####
setwd("~/Desktop/Master degree QF/Notes/RiskMeasures")
source("VaR class.R")

## Load stock data
initialData <- pdfetch::pdfetch_YAHOO("^GSPC",fields="close",from="1993-01-01",to="2018-09-28")
P. <- zoo::coredata(initialData) ## Closing prices
R. <- -diff(P.) ## Time index
t. <- zoo::index(initialData)[-1] ## Relative returns (losses)

## Create object with underlying data
x <- VaR$new(R., t.)

## Add types of VaR you want
x$historical(alpha = 0.99, lookback = 250)
x$weighted(alpha = 0.99, lam = 0.97, lookback = 250)
x$gaussian(alpha = 0.99, lookback = 250)

## Visualize the data + VaRs
x$Plot()
