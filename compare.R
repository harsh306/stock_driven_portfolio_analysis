
library(quantmod);library(tseries);
library(timeSeries);library(forecast);library(xts);

start = as.Date("2016-01-01")
end = as.Date("2016-10-01")

## Plotting the closing prices
getSymbols("ANIP",src = "yahoo",from = start, to= end)
plot(ANIP.Date~ANIP.Adjusted,main = "ANIP")

