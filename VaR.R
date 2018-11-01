library(R6)

VaR <- R6Class("VaR",list(
  Var = list(),
  R. = 0,
  t. = 0,
  initialize = function(data, timeIndex){
    self$R. <- data
    self$t. <- timeIndex
    
    setwd("~/Desktop/Master degree QF/Notes/RiskMeasures/VaR functions")
    source("VaR.historical.R")
    source("VaR.weighted.R")
    source("VaR.kernel.R")
    source("VaR.gaussian.R")
  },
  Plot = function(){
    k <- 2
    plot(x = self$t., y = self$R., type = "l", col = "lightgray",
         xlab = "time", ylab = expression(VaR[alpha]),
         main = "VaR time-series")
    for (element in self$Var){
      points(x = element$time,
             y = element$value, type = "l", col = k)
      k <- k + 1
    }
    if (length(self$Var) != 0)
      legend("topleft", legend = names(self$Var),
             col = 2:(length(self$Var) + 1), lty = rep(1, length(self$Var)), cex = 0.6,
             bty = "n",  y.intersp = 0.5)
  },
  Delete = function(name){
    self$Val[[name]] <- NULL
  },
  historical = function(...){
    Value <- VaR.historical(data = self$R., ...)
    lookback <- length(self$R.) - length(Value) + 1
    Time <- c(self$t.[(lookback + 1):length(self$R.)], self$t.[length(self$R.)] + 1)
    self$Var[["historical"]] <- data.frame("time" = Time,
                                           "value" = Value)
  },
  weighted = function(...){
    Value <- VaR.weighted(data = self$R., ...)
    lookback <- length(self$R.) - length(Value) + 1
    Time <- c(self$t.[(lookback + 1):length(self$R.)], self$t.[length(self$R.)] + 1)
    self$Var[["weighted"]] <- data.frame("time" = Time,
                                         "value" = Value)
  },
  kernel = function(...){
    Value <- VaR.kernel(data = self$R., ...)
    lookback <- length(self$R.) - length(Value) + 1
    Time <- c(self$t.[(lookback + 1):length(self$R.)], self$t.[length(self$R.)] + 1)
    self$Var[["kernel"]] <- data.frame("time" = Time,
                                       "value" = Value)
  },
  gaussian = function(...){
    Value <- VaR.gaussian(data = self$R., ...)
    lookback <- length(self$R.) - length(Value) + 1
    Time <- c(self$t.[(lookback + 1):length(self$R.)], self$t.[length(self$R.)] + 1)
    self$Var[["gaussian"]] <- data.frame("time" = Time,
                                         "value" = Value)
  }
))

