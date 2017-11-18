library(quantmod);library(tseries);
library(timeSeries);library(forecast);library(xts);

start = as.Date("2013-01-01")  
end = as.Date("2016-01-31")

# Starting with SLR
getSymbols("ANIP",src = "yahoo",from=start,to=end)
ANIP.df = data.frame(Date = index(ANIP),coredata(ANIP))
l = lm(ANIP.Adjusted~Date,data = ANIP.df)
summary(l)
attach(ANIP.df)
plot(Date,ANIP.Adjusted,las=1,pch=20)
abline(l,lwd = 3,col="red")


#Continuing to MLR

