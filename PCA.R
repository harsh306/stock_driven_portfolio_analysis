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

a = c(allData$ANIP.Adjusted[1],allData$ANIP.Adjusted[1:721])
allData['ANIP.Predict'] = a
attach(allData)

#newData 
b = data.frame(allData[1,c(-1,-7,-17)])
b[2:722,] = allData[1:721,c(-1,-7,-17)]
newData = data.frame(allData[,c(1,7,17)],b)
train_new = newData[1:361,]
test_new = newData[-(1:361),]

pca_fit = prcomp(train_new[2:16])
par(mfrow=c(1,1))
plot(pca_fit$x[, 1:2], col= 1:2,xlab = "Z1", ylab = "Z2",  pch=20,las=1)
fit_pcalm = lm(train_new$ANIP.Adjusted~pca_fit$x[,1:2],data = as.data.frame(pca_fit$x))
summary.lm(fit_pcalm)
pred_pcr = predict(fit_pcalm,test_new)
accuracy(pred_pcr,test_new$ANIP.Adjusted)
plot(test_new$Date,test_new$ANIP.Adjusted,las=1,pch=20,col = "grey",type="l")
lines(test_new$Date,fitted(fit_pcalm),lwd = 3,col="red")
axis.Date(1,at= seq(test_new$Date[1],test_new$Date[361],length.out = 10),format = "%Y-%m",las=2)
plot(test_new$Date,test_new$ANIP.Adjusted,las=1,lwd=2,xaxt = 'n',type="l",pch=22,ylab="Close Prices",xlab="Date",main="Results Using PCA : ANIP")
lines(test_new$Date,pred_pcr,col="red",lwd=2,pch=22)
legend("topright",legend = c("Actual CP","Predicted CP"),col=c("black","red"),lty=1:1)


