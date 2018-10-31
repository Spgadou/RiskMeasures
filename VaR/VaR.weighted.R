# alpha: risk level
# data: data to compute VaR
# lam: exponential weight factor of most recent data point
#      (i.e. oldest data point gets weight lam^(lookback))
# h: bandwidth of Kernel density function
# lookback: lookback period for VaR time series

VaR.weighted <- function(alpha, data, lam = 0.99, lookback = length(data)){
  n <- length(data)
  vk <- 1:(n - lookback + 1)
  w <- sapply(1:lookback, function(i) lam^(lookback - i + 1)); w <- w / sum(w)
  sapply(vk, function(i){
    selectedR. <- data[i:(i + lookback - 1)]
    ranking <- order(selectedR.)
    sort(selectedR., decreasing = FALSE)[min(which(cumsum(w[ranking]) >= alpha))]
  })
}

# initialData <- pdfetch::pdfetch_YAHOO("^GSPC",fields="close",from="1993-01-01",to="2018-09-28")
# P. <- zoo::coredata(initialData) ## Closing prices
# R. <- -diff(P.) ## Time index
# t. <- zoo::index(initialData) ## Relative returns (losses)
# 
# VaR <- VaR.weighted(0.99, R., lam = 0.97, lookback = 250)
# plot(x = head(t., -1)[-(1:250)], y = R.[-(1:250)], type = "l",
#      col = "lightgray")
# points(x = t.[-(1:250)], y = VaR, type = "l", col = 2)
