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
l = lm(train_new$BAC.Adjusted~train_new$BAC.Predict)
summary(l)

pred = predict(l,test_new)
accuracy(pred,test_new$BAC.Adjusted)
axis.Date(1,at= seq(test_new$Date[1],test_new$Date[361],length.out = 10),format = "%Y-%m",las=2)
plot(test_new$Date,test_new$BAC.Predict,col="blue",las=1,lwd=2,xaxt = 'n',type="l",pch=22,ylab="Close Prices",xlab="Date",main="Results Using SLR : BAC")
lines(test_new$Date,pred,col="red",lwd=2,pch=22)
legend("topleft",legend = c("Actual CP","Predicted CP"),col=c("blue","red"),lty=1:1)


#Continue MLR
lm.fit = lm(BAC.Adjusted ~.,data = train_new)
summary(lm.fit)
pred_mlr = predict(lm.fit,test_new)
accuracy(pred_mlr,test_new$BAC.Adjusted)
axis.Date(1,at= seq(test_new$Date[1],test_new$Date[361],length.out = 10),format = "%Y-%m",las=2)
plot(test_new$Date,test_new$BAC.Predict,col="blue",las=1,lwd=2,xaxt = 'n',type="l",pch=22,ylab="Close Prices",xlab="Date",main="Results Using MLR : BAC")
lines(test_new$Date,pred_mlr,col="red",lwd=2,pch=22)
legend("topleft",legend = c("Actual CP","Predicted CP"),col=c("blue","red"),lty=1:1)


library(randomForest)
rf = randomForest(BAC.Adjusted ~BAC.Open+BAC.Close+BAC.High+BAC.Low+sma20+rsi14+BAC.Predict,data = train_new,mytry=5,importance=TRUE)
rf$importance
pred_rf = predict(rf,test_new)
accuracy(pred_rf,test_new$BAC.Adjusted)
axis.Date(1,at= seq(test_new$Date[1],test_new$Date[361],length.out = 10),format = "%Y-%m",las=2)
plot(test_new$Date,test_new$BAC.Predict,col="blue",las=1,lwd=2,xaxt = 'n',type="l",pch=22,ylab="Close Prices",xlab="Date",main="Results Using Random Forests : BAC")
lines(test_new$Date,pred_rf,col="red",lwd=2,pch=22)
legend("topleft",legend = c("Actual CP","Predicted CP"),col=c("blue","red"),lty=1:1)

#GAM Splines
#fit_gam <- gam(BAC.filter1$BAC.predict ~ s(BAC.filter1$BAC.Open,4)+s(BAC.filter1$BAC.High,4)+s(BAC.filter1$BAC.Low,4)+s(BAC.filter1$BAC.Volume,4)+s(BAC.filter1$BAC.Adjusted,4)+s(BAC.filter1$SMA,4)+s(BAC.filter1$RST,4)+s(BAC.filter1$CCI,4))
library(gam)
fit_gam <- gam(BAC.Adjusted ~. , data = train_new)
summary.gam(fit_gam)
pred_gam = predict.gam(fit_gam,test_new)
accuracy(pred_gam,test_new$BAC.Adjusted)
axis.Date(1,at= seq(test_new$Date[1],test_new$Date[361],length.out = 10),format = "%Y-%m",las=2)
plot(test_new$Date,test_new$BAC.Predict,col="blue",las=1,lwd=2,xaxt = 'n',type="l",pch=22,ylab="Close Prices",xlab="Date",main="Results Using GAM : BAC")
lines(test_new$Date,pred_gam,col="red",lwd=2,pch=22)
legend("topleft",legend = c("Actual CP","Predicted CP"),col=c("blue","red"),lty=1:1)


#PCA for BAC
pca_fit = prcomp(train_new[2:16])
par(mfrow=c(1,1))
plot(pca_fit$x[, 1:2], col= 1:2,xlab = "Z1", ylab = "Z2",  pch=20,las=1)
fit_pcalm = lm(train_new$BAC.Adjusted~pca_fit$x[,1:2],data = as.data.frame(pca_fit$x))
summary.lm(fit_pcalm)
pred_pcr = predict(fit_pcalm,test_new)
accuracy(pred_pcr,test_new$BAC.Adjusted)

axis.Date(1,at= seq(test_new$Date[1],test_new$Date[361],length.out = 10),format = "%Y-%m",las=2)
plot(test_new$Date,test_new$BAC.Adjusted,las=1,col="blue",lwd=2,xaxt = 'n',type="l",pch=22,ylab="Close Prices",xlab="Date",main="Results Using PCA : BAC")
lines(test_new$Date,pred_pcr,col="red",lwd=2,pch=22)
legend("topleft",legend = c("Actual CP","Predicted CP"),col=c("blue","red"),lty=1:1)


# SMooth Splines
fit_spline <- smooth.spline(train_new$BAC.Predict , train_new$BAC.Adjusted)
pred_spline = predict(fit_spline,test_new$BAC.Adjusted)

accuracy(pred_spline$y,test_new$BAC.Adjusted)

axis.Date(1,at= seq(test_new$Date[1],test_new$Date[361],length.out = 10),format = "%Y-%m",las=2)
plot(test_new$Date,test_new$BAC.Adjusted,las=1,col="blue",lwd=2,xaxt = 'n',type="l",pch=22,ylab="Close Prices",xlab="Date",main="Results Using Smooth Splines : BAC")
lines(test_new$Date,pred_spline$y,col="red",lwd=2,pch=22)
legend("topleft",legend = c("Actual CP","Predicted CP"),col=c("blue","red"),lty=1:1)
