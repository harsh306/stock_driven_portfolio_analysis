library(quantmod);library(tseries);
library(timeSeries);library(forecast);library(xts);
library(TTR)

start = as.Date("2014-11-29")  
end = as.Date("2017-11-29")

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

a = c(BAC.df$BAC.Adjusted[1],BAC.df$BAC.Adjusted[1:754])
allData["BAC.Predict"] = a
attach(allData)
allData = na.omit(allData)
train_data = allData[1:361,]
test_data = allData[-(1:361),]
plot(test_data$ANIP.Adjusted~test_data$ANIP.Adjusted)
plot(train_data$ANIP.Adjusted~train_data$ANIP.Predict)
l = lm(train_data$BAC.Adjusted~train_data$BAC.Predict)
summary(l)

pred = predict(l,test_data)
accuracy(pred,test_data$BAC.Adjusted)
plot(test_data$Date,test_data$ANIP.Predict,las=1,type="l",pch=20)
lines(test_data$Date,pred,col="red")


#Continue MLR
lm.fit = lm(BAC.Predict ~.,data = train_data)
summary(lm.fit)
pred_mlr = predict(lm.fit,test_data)
accuracy(pred_mlr,test_data$BAC.Adjusted)

