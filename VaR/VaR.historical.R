# alpha: risk level
# data: data to compute VaR
# lookback: lookback period for VaR time series

VaR.historical <- function(alpha, data, lookback = length(data)){
  n <- length(data)
  vk <- 1:(n - lookback + 1)
  sapply(vk, function(i) quantile(data[i:(i + lookback - 1)], alpha, type = 1))
}

# initialData <- pdfetch::pdfetch_YAHOO("^GSPC",fields="close",from="1993-01-01",to="2018-09-28")
# P. <- zoo::coredata(initialData) ## Closing prices
# R. <- -diff(P.) / head(P., -1) ## Time index
# t. <- zoo::index(initialData) ## Relative returns (losses)
# 
# VaR <- VaR.historical(0.99, R., lookback = 250)
# plot(x = head(t., -1)[-(1:250)], y = R.[-(1:250)], type = "l",
#      col = "lightgray")
# points(x = t.[-(1:250)], y = VaR, type = "l", col = 2)
