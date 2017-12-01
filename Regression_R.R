library(quantmod);library(tseries);
library(timeSeries);library(forecast);library(xts);
library(TTR)

start = as.Date("2014-11-29")  
end = as.Date("2017-11-29")

# Getting the Data Frame for ANIP Stocks
getSymbols("ANIP",src = "yahoo",from=start,to=end)

ANIP.df = data.frame(Date = index(ANIP),coredata(ANIP))

#Getting new features using TTR Library
sma20 = SMA(ANIP.df[c("ANIP.Adjusted")],n = 20)
ema14 = EMA(ANIP.df[c("ANIP.Adjusted")],n=14)
bb20 = BBands(ANIP.df[c("ANIP.Adjusted")],sd = 2.0)
dataWithBB = data.frame(ANIP.df,bb20)

#Parameter RSI
rsi14 = RSI(ANIP.df[c("ANIP.Adjusted")],n=14)
macd = MACD(ANIP.df[c("ANIP.Adjusted")], nFast=12, nSlow=26, nSig=9, maType=SMA)

allData = data.frame(ANIP.df,sma20,ema14,bb20,rsi14,macd)
allData = na.omit(allData)

allData['ANIP.Predict'] = a
attach(allData)

train_data = allData[1:361,]
test_data = allData[-(1:361),]
plot(test_data$ANIP.Adjusted~test_data$ANIP.Adjusted)
plot(train_data$ANIP.Adjusted~train_data$ANIP.Predict)
l = lm(train_data$ANIP.Adjusted~train_data$ANIP.Predict)
summary(l)

pred = predict(l,test_data)
accuracy(pred,test_data$ANIP.Adjusted)
plot(test_data$Date,test_data$ANIP.Predict,las=1,type="l",pch=20)
lines(test_data$Date,pred,col="red")

#Now Continue to MLR
lm.fit = lm(ANIP.Predict ~.,data = train_data)
summary(lm.fit)
pred_mlr = predict(lm.fit,test_data)
accuracy(pred,test_data$ANIP.Adjusted)
plot(test_data$Date,test_data$ANIP.Predict,las=1,type="l",pch=20)
lines(test_data$Date,pred_mlr,col="red")

test_data$ANIP.Adjusted[358:361]
pred_mlr[358:361]
