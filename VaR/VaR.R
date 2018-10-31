setwd("~/Desktop/Master degree QF/Notes/RiskMeasures/VaR")
source("VaR.historical.R")
source("VaR.weighted.R")
source("VaR.kernel.R")

VaR <- list("historical" = VaR.historical,
            "weighted" = VaR.weighted,
            "kernel" = VaR.kernel)

# initialData <- pdfetch::pdfetch_YAHOO("^GSPC",fields="close",from="1993-01-01",to="2018-09-28")
# P. <- zoo::coredata(initialData) ## Closing prices
# R. <- -diff(P.) ## Time index
# t. <- zoo::index(initialData) ## Relative returns (losses)
# 
# ValuesAtRisk <- VaR$weighted(0.99, R., lam = 0.97, lookback = 250)
# plot(x = head(t., -1)[-(1:250)], y = R.[-(1:250)], type = "l",
#      col = "lightgray")
# points(x = t.[-(1:250)], y = ValuesAtRisk, type = "l", col = 2)
