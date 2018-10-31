VaR.kernel <- function(alpha, data, Kernel, h = 0.01, lookback = length(data)){
  n <- length(data)
  vk <- 1:(n - lookback + 1); vk2 <- 1:lookback
  if (!is.numeric(try(Kernel(1:2), silent = TRUE))){
    Kernel <- Vectorize(Kernel)
  }
  sapply(vk, function(i){
    sortedR. <- sort(data[i:(i + lookback - 1)])
    sum(Kernel((vk2 / lookback - alpha) / h) * sortedR.) / (h * lookback)
  })
}

# kernelFunction <- function(x){
#   0.5 * (x >= -1) * (x <= 1)
# }
#   
# initialData <- pdfetch::pdfetch_YAHOO("^GSPC",fields="close",from="1993-01-01",to="2018-09-28")
# P. <- zoo::coredata(initialData) ## Closing prices
# R. <- -diff(P.) ## Time index
# t. <- zoo::index(initialData) ## Relative returns (losses)
# 
# VaR <- VaR.kernel(0.99, R., Kernel = kernelFunction, h = 0.1, lookback = 250)
# plot(x = head(t., -1)[-(1:250)], y = R.[-(1:250)], type = "l",
#      col = "lightgray")
# points(x = t.[-(1:250)], y = VaR, type = "l", col = 2)


