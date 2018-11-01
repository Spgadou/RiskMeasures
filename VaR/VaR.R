setwd("~/Desktop/Master degree QF/Notes/RiskMeasures/VaR")
source("VaR.historical.R")
source("VaR.weighted.R")
source("VaR.kernel.R")

VaR <- list("historical" = VaR.historical,
            "weighted" = function(alpha, data, ...){
              VaR.weighted(alpha, data, lam = 0.97, ...)
            },
            "kernel" = function(alpha, data, ...){
              VaR.kernel(alpha, data, Kernel = function(x){
                0.5 * (x >= -1) * (x <= 1)
              }, h = 0.02, ...)
            })

initialData <- pdfetch::pdfetch_YAHOO("^GSPC",fields="close",from="1993-01-01",to="2018-09-28")
P. <- zoo::coredata(initialData) ## Closing prices
R. <- -diff(P.) ## Time index
t. <- zoo::index(initialData) ## Relative returns (losses)

ValuesAtRisk <- sapply(c("historical", "weighted", "kernel"),
                       function(type) VaR[[type]](0.99, R., lookback = 250))

plot(x = head(t., -1)[-(1:250)], y = R.[-(1:250)], type = "l",
     col = "lightgray", main = "VaR estimated for S&P 500",
     xlab = "time", ylab = expression(R[abs]))
for (i in 1:dim(ValuesAtRisk)[2]){
  points(x = t.[-(1:250)], y = ValuesAtRisk[,i], type = "l", col = i + 1)
}
legend("topleft", legend = c("historical", expression('weighted (' ~ lambda ~ '= 0.97)'),
                             expression('kernel ( h = 0.01)')),
       col = c(2, 3, 4), lty = rep(1, 3), cex = 0.6,
       bty = "n",  y.intersp = 0.5)

