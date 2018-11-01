# alpha: risk level
# data: data to compute VaR
# lam: exponential weight factor of most recent data point
#      (i.e. oldest data point gets weight lam^(lookback))
# lookback: lookback period for VaR time series

VaR.gaussian <- function(alpha, data, lam = 0.99, lookback = length(data)){
  n <- length(data)
  vk <- 1:(n - lookback + 1)
  w <- sapply(1:lookback, function(i) lam^(lookback - i + 1)); w <- w / sum(w)
  sapply(vk, function(i){
    selectedR. <- data[i:(i + lookback - 1)]
    mu <- mean(selectedR.)
    sigma <- sqrt(sum(w * (selectedR. - mu)^2))
    mu + sigma * qnorm(alpha)
  })
}