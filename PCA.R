library(quantmod);library(tseries);
library(timeSeries);library(forecast);library(xts);
library(TTR)

start = as.Date("2014-11-29")  
end = as.Date("2017-11-29")

# Getting the Data Frame for ANIP Stocks
getSymbols("ANIP",src = "yahoo",from=start,to=end)
ANIP.df = data.frame(Date = index(ANIP),coredata(ANIP))

sma20 = SMA(ANIP.df[c("ANIP.Adjusted")],n = 20)
ema14 = EMA(ANIP.df[c("ANIP.Adjusted")],n=14)
bb20 = BBands(ANIP.df[c("ANIP.Adjusted")],sd = 2.0)
dataWithBB = data.frame(ANIP.df,bb20)

#Parameter RSI
rsi14 = RSI(ANIP.df[c("ANIP.Adjusted")],n=14)
macd = MACD(ANIP.df[c("ANIP.Adjusted")], nFast=12, nSlow=26, nSig=9, maType=SMA)

allData = data.frame(ANIP.df,sma20,ema14,bb20,rsi14,macd)
allData = na.omit(allData)
train_data = allData[1:361,]
test_data = allData[-(1:361),]
pca_fit = prcomp(train_data[2:16])
par(mfrow=c(1,1))
plot(pca_fit$x[, 1:2], col= 1:2,xlab = "Z1", ylab = "Z2",  pch=20,las=1)
fit_pcalm = lm(train_data$ANIP.Adjusted~pca_fit$x[,1:2],data = as.data.frame(pca_fit$x))
summary.lm(fit_pcalm)
pred_pcr = predict(fit_pcalm,test_data)
accuracy(pred_pcr,test_data$ANIP.Adjusted)
plot(test_data$Date,test_data$ANIP.Adjusted,las=1,pch=20,col = "grey",type="l")
lines(test_data$Date,fitted(fit_pcalm),lwd = 3,col="red")


