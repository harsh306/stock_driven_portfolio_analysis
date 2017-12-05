library(quantmod);library(tseries);
library(timeSeries);library(forecast);library(xts);
library(TTR)

saxis.Date(1,at= seq(test_new$Date[1],test_new$Date[361],length.out = 10),format = "%Y-%m",las=2)
plot(test_new$Date,test_new$BAC.Predict,col="blue",las=1,lwd=2,xaxt = 'n',type="l",pch=22,ylab="Close Prices",xlab="Date",main="Results Using SLR : BAC")
lines(test_new$Date,pred,col="red",lwd=2,pch=22)
legend("topleft",legend = c("Actual CP","Predicted CP"),col=c("blue","red"),lty=1:1)
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
l = lm(train_new$ANIP.Adjusted~train_new$ANIP.Predict)
summary(l)

pred = predict(l,test_new)
accuracy(pred,test_new$ANIP.Adjusted)
axis.Date(1,at= seq(test_new$Date[1],test_new$Date[361],length.out = 10),format = "%Y-%m",las=2)
plot(test_new$Date,test_new$ANIP.Predict,las=1,lwd=2,xaxt = 'n',type="l",pch=22,ylab="Close Prices",xlab="Date",main="Results Using SLR")
lines(test_new$Date,pred,col="red",lwd=2,pch=22)
legend("topright",legend = c("Actual CP","Predicted CP"),col=c("black","red"),lty=1:1)

#Now Continue to MLR
lm.fit = lm(ANIP.Adjusted ~ANIP.Open+ANIP.High+ANIP.Low+sma20+rsi14,data = train_new)
summary(lm.fit)
pred_mlr = predict(lm.fit,test_new)
accuracy(pred_mlr,test_new$ANIP.Adjusted)
axis.Date(1,at= seq(test_new$Date[1],test_new$Date[361],length.out = 10),format = "%Y-%m",las=2)
plot(test_new$Date,test_new$ANIP.Predict,las=1,lwd=2,xaxt = 'n',type="l",pch=22,ylab="Close Prices",xlab="Date",main="Results Using MLR : ANIP")
lines(test_new$Date,pred_mlr,col="red",lwd=2,pch=22)
legend("topright",legend = c("Actual CP","Predicted CP"),col=c("black","red"),lty=1:1)


#Smooth Splines
fit_spline <- smooth.spline(train_new$ANIP.Predict , train_new$ANIP.Adjusted,cv = 10)
pred_spline = predict(fit_spline,test_new$ANIP.Adjusted)

accuracy(pred_spline$y,test_new$ANIP.Adjusted)

axis.Date(1,at= seq(test_new$Date[1],test_new$Date[361],length.out = 10),format = "%Y-%m",las=2)
plot(test_new$Date,test_new$ANIP.Predict,las=1,lwd=2,xaxt = 'n',type="l",pch=22,ylab="Close Prices",xlab="Date",main="Results Using Splines : ANIP")
lines(test_new$Date,pred_spline$y,col="red",lwd=2,pch=22)
legend("topright",legend = c("Actual CP","Predicted CP"),col=c("black","red"),lty=1:1)


#Random Forests
library(randomForest)
rf = randomForest(ANIP.Adjusted ~ANIP.Open+ANIP.Close+ANIP.High+ANIP.Low+sma20+rsi14+ANIP.Predict,data = train_new,mytry=5,importance=TRUE)
pred_rf = predict(rf,test_new)
accuracy(pred_rf,test_new$ANIP.Adjusted)
plot(rf)
pred_rf[-2]
axis.Date(1,at= seq(test_new$Date[1],test_new$Date[361],length.out = 10),format = "%Y-%m",las=2)
plot(test_new$Date,test_new$ANIP.Predict,las=1,lwd=2,xaxt = 'n',type="l",pch=22,ylab="Close Prices",xlab="Date",main="Results Using Random Forests : ANIP")
lines(test_new$Date,pred_rf,col="red",lwd=2,pch=22)
legend("topright",legend = c("Actual CP","Predicted CP"),col=c("black","red"),lty=1:1)

# GAM Splines
library(gam)
fit_gam <- gam(ANIP.Adjusted ~. , data = train_new)
summary.gam(fit_gam)
pred_gam = predict.gam(fit_gam,test_new)
accuracy(pred_gam,test_new$ANIP.Adjusted)
axis.Date(1,at= seq(test_new$Date[1],test_new$Date[361],length.out = 10),format = "%Y-%m",las=2)
plot(test_new$Date,test_new$ANIP.Predict,las=1,lwd=2,xaxt = 'n',type="l",pch=22,ylab="Close Prices",xlab="Date",main="Results Using GAM : ANIP")
lines(test_new$Date,pred,col="red",lwd=2,pch=22)
legend("topright",legend = c("Actual CP","Predicted CP"),col=c("black","red"),lty=1:1)

