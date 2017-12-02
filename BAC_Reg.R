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

a = c(BAC.df$BAC.Adjusted[1],BAC.df$BAC.Adjusted[1:721])
allData["BAC.Predict"] = a
b = data.frame(allData[1,c(-1,-7,-17)])
b[2:722,] = allData[1:721,c(-1,-7,-17)]
newData = data.frame(allData[,c(1,7,17)],b)
train_new = newData[1:361,]
test_new = newData[-(1:361),]


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
lm.fit = lm(BAC.Predict ~.,data = train_new)
summary(lm.fit)
pred_mlr = predict(lm.fit,test_new)
accuracy(pred_mlr,test_new$BAC.Adjusted)

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
#plot(fit_gam,se = TRUE, pages= 12,all.terms=TRUE,scheme=1)

