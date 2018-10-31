initialData <- pdfetch::pdfetch_YAHOO("^GSPC",fields="close",from="1993-01-01",to="2018-09-28")
P. <- zoo::coredata(initialData) ## Closing prices
R. <- -diff(P.) ## Time index
t. <- zoo::index(initialData) ## Relative returns (losses)