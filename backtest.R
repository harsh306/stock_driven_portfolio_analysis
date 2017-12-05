library(quantmod);library(tseries);
library(timeSeries);library(forecast);library(xts);
library(TTR)

start = as.Date("2005-09-15")  
end = as.Date("2008-09-20")

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
a = c(allData$ANIP.Adjusted[1],allData$ANIP.Adjusted[1:725])
allData['ANIP.Predict'] = a
attach(allData)

#newData 
b = data.frame(allData[1,c(-1,-7,-17)])
b[2:726,] = allData[1:725,c(-1,-7,-17)]
newData = data.frame(allData[,c(1,7,17)],b)
train_new = newData[1:363,]
test_new = newData[-(1:363),]

l = lm(train_new$ANIP.Adjusted~train_new$ANIP.Predict)
summary(l)

pred = predict(l,test_new)
accuracy(pred,test_new$ANIP.Adjusted)
plot(test_data$Date,test_data$ANIP.Predict,las=1,type="l",pch=22,xlab="DATE",ylab="Close Prices",main="Results Using SLR")
lines(test_data$Date,pred,col="red",pch=22)

lm.fit = lm(ANIP.Adjusted ~ANIP.Open+ANIP.High+ANIP.Low+sma20+rsi14,data = train_new)
summary(lm.fit)
pred_mlr = predict(lm.fit,test_new)
accuracy(pred_mlr,test_new$ANIP.Adjusted)
plot(test_data$Date,test_data$ANIP.Adjusted,las=1,type="l",pch=20)
lines(test_data$Date,pred_mlr,col="red")

library(gam)
fit_gam <- gam(ANIP.Adjusted ~. , data = train_new)
summary.gam(fit_gam)
pred_gam = predict.gam(fit_gam,test_new)
accuracy(pred_gam,test_new$ANIP.Adjusted)

library(randomForest)
rf = randomForest(ANIP.Adjusted ~ANIP.Open+ANIP.Close+ANIP.High+ANIP.Low+sma20+rsi14+ANIP.Predict,data = train_new,mytry=5,importance=TRUE)
pred_rf = predict(rf,test_new)
accuracy(pred_rf,test_new$ANIP.Adjusted)

# Bank of America
library(quantmod);library(tseries);
library(timeSeries);library(forecast);library(xts);
library(TTR)

start = as.Date("2005-09-15")  
end = as.Date("2008-09-20")

# Getting the Data Frame for ANIP Stocks
getSymbols("BAC",src = "yahoo",from=start,to=end)
BAC.df  =data.frame(Date = index(BAC),coredata(BAC))
#Getting new features using TTR Library
sma20 = SMA(BAC.df[c("BAC.Adjusted")],n = 20)
ema14 = EMA(BAC.df[c("BAC.Adjusted")],n=14)
bb20 = BBands(BAC.df[c("BAC.Adjusted")],sd = 2.0)
dataWithBB = data.frame(BAC.df,bb20)

#Parameter RSI
rsi14 = RSI(BAC.df[c("BAC.Adjusted")],n=14)
macd = MACD(BAC.df[c("BAC.Adjusted")], nFast=12, nSlow=26, nSig=9, maType=SMA)

allData = data.frame(BAC.df,sma20,ema14,bb20,rsi14,macd)
allData = na.omit(allData)

a = c(BAC.df$BAC.Adjusted[1],BAC.df$BAC.Adjusted[1:725])
allData["BAC.Predict"] = a
b = data.frame(allData[1,c(-1,-7,-17)])
b[2:726,] = allData[1:725,c(-1,-7,-17)]
newData = data.frame(allData[,c(1,7,17)],b)
train_new = newData[1:363,]
test_new = newData[-(1:363),]

library(randomForest)
rf = randomForest(BAC.Adjusted ~BAC.Open+BAC.Close+BAC.High+BAC.Low+sma20+rsi14+BAC.Predict,data = train_new,mytry=5,importance=TRUE)
pred_rf = predict(rf,test_new)
accuracy(pred_rf,test_new$BAC.Adjusted)
plot(rf)
pred_rf[-2]

# GAM Splines

#fit_gam <- gam(BAC.filter1$BAC.predict ~ s(BAC.filter1$BAC.Open,4)+s(BAC.filter1$BAC.High,4)+s(BAC.filter1$BAC.Low,4)+s(BAC.filter1$BAC.Volume,4)+s(BAC.filter1$BAC.Adjusted,4)+s(BAC.filter1$SMA,4)+s(BAC.filter1$RST,4)+s(BAC.filter1$CCI,4))
library(gam)
fit_gam <- gam(BAC.Adjusted ~. , data = train_new)
summary.gam(fit_gam)
pred_gam = predict.gam(fit_gam,test_new)
accuracy(pred_gam,test_new$BAC.Adjusted)
par(mfrow=c(1,12))
pred_gam

# Microsoft
getSymbols("MSFT",src = "yahoo",from=start,to=end)

MSFT.df = data.frame(Date = index(MSFT),coredata(MSFT))

#Getting new features using TTR Library
sma20 = SMA(MSFT.df[c("MSFT.Adjusted")],n = 20)
ema14 = EMA(MSFT.df[c("MSFT.Adjusted")],n=14)
bb20 = BBands(MSFT.df[c("MSFT.Adjusted")],sd = 2.0)
dataWithBB = data.frame(MSFT.df,bb20)

#Parameter RSI
rsi14 = RSI(MSFT.df[c("MSFT.Adjusted")],n=14)
macd = MACD(MSFT.df[c("MSFT.Adjusted")], nFast=12, nSlow=26, nSig=9, maType=SMA)

allData = data.frame(MSFT.df,sma20,ema14,bb20,rsi14,macd)
allData = na.omit(allData)
a = c(allData$MSFT.Adjusted[1],allData$MSFT.Adjusted[1:725])
allData['MSFT.Predict'] = a
attach(allData)

#newData 
b = data.frame(allData[1,c(-1,-7,-17)])
b[2:726,] = allData[1:725,c(-1,-7,-17)]
newData = data.frame(allData[,c(1,7,17)],b)
train_new = newData[1:363,]
test_new = newData[-(1:363),]

l = lm(train_new$MSFT.Adjusted~train_new$MSFT.Predict)
summary(l)

pred = predict(l,test_new)
accuracy(pred,test_new$MSFT.Adjusted)


lm.fit = lm(MSFT.Adjusted ~MSFT.Open+MSFT.High+MSFT.Low+sma20+rsi14,data = train_new)
summary(lm.fit)
pred_mlr = predict(lm.fit,test_new)
accuracy(pred_mlr,test_new$MSFT.Adjusted)

library(gam)
fit_gam <- gam(MSFT.Adjusted ~. , data = train_new)
summary.gam(fit_gam)
pred_gam = predict.gam(fit_gam,test_new)
accuracy(pred_gam,test_new$MSFT.Adjusted)

library(randomForest)
rf = randomForest(MSFT.Adjusted ~MSFT.Open+MSFT.Close+MSFT.High+MSFT.Low+sma20+rsi14+MSFT.Predict,data = train_new,mytry=5,importance=TRUE)
pred_rf = predict(rf,test_new)
accuracy(pred_rf,test_new$MSFT.Adjusted)
