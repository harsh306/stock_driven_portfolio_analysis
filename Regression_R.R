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
a = c(allData$ANIP.Adjusted[1],allData$ANIP.Adjusted[1:721])
allData['ANIP.Predict'] = a
attach(allData)

#newData 
b = data.frame(allData[1,c(-1,-7,-17)])
b[2:722,] = allData[1:721,c(-1,-7,-17)]
newData = data.frame(allData[,c(1,7,17)],b)
train_new = newData[1:361,]
test_new = newData[-(1:361),]

train_data = allData[1:361,]
test_data = allData[-(1:361),]
l = lm(train_data$ANIP.Adjusted~train_data$ANIP.Predict)
summary(l)

pred = predict(l,test_data)
accuracy(pred,test_data$ANIP.Adjusted)
plot(test_data$Date,test_data$ANIP.Predict,las=1,type="l",pch=22,xlab="DATE",ylab="Close Prices",main="Results Using SLR")
lines(test_data$Date,pred,col="red",pch=22)

#Now Continue to MLR
lm.fit = lm(ANIP.Adjusted ~ANIP.Open+ANIP.High+ANIP.Low+sma20+rsi14,data = train_new)
summary(lm.fit)
pred_mlr = predict(lm.fit,test_new)
accuracy(pred_mlr,test_new$ANIP.Adjusted)
plot(test_data$Date,test_data$ANIP.Adjusted,las=1,type="l",pch=20)
lines(test_data$Date,pred_mlr,col="red")

#GAM Splines
fit_spline <- smooth.spline(train_new$ANIP.Predict , train_data$ANIP.Adjusted)
pred_spline = predict(fit_spline,test_data$ANIP.Adjusted)

accuracy(pred_spline$y,test_data$ANIP.Adjusted)

plot(test_data$Date,test_data$ANIP.Adjusted,type="l",pch=22)
lines(test_new$Date,pred_spline$y,col="red")


#Random Forests
library(randomForest)
rf = randomForest(ANIP.Adjusted ~ANIP.Open+ANIP.Close+ANIP.High+ANIP.Low+sma20+rsi14+ANIP.Predict,data = train_new,mytry=5,importance=TRUE)
pred_rf = predict(rf,test_new)
accuracy(pred_rf,test_new$ANIP.Adjusted)
plot(rf)
pred_rf[-2]
