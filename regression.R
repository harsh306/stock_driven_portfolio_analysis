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

a <- allData[2:length(allData$ANIP.Adjusted),]$ANIP.Adjusted
allData['ANIP.Predict'] <- c(a,allData[length(allData$ANIP.Adjusted),7])

#Normalizing the Dataset
#for(i in c(2:length(allData))){
# allData[i] <- scale(allData[i])
#}

#Performing SLR (Yt = Yt-1 + Error)
l = lm(ANIP.Adjusted~ANIP.Predict,data = train_data)
summary(l)
plot(train_data$Date,train_data$ANIP.Predict,las=1,pch=20)

pred_slr = predict(l,test_data)
plot(test_data$Date,test_data$ANIP.Predict,las=1,type="l",pch=20)
lines(test_data$Date,pred_slr,col="red")

attach(allData)

#Continuing to MLR
train_data = allData[1:361,]
test_data = allData[-(1:361),]
train_ANIP = train_data[,-7]
lm.fit = lm(ANIP.Predict ~.,data = train_data)
summary(lm.fit)
lm.fit2 = lm(ANIP.Predict~+pctB+rsi14+rsi14+sma20+macd+ema14+ANIP.Open+ANIP.High+ANIP.Low+mavg+dn+up,data = train_data)
summary(lm.fit2)
pred = predict(lm.fit2,test_data)
accuracy(pred,test_data$ANIP.Adjusted)
plot(test_data$Date,test_data$ANIP.Predict,las=1,type="l",pch=20)
lines(test_data$Date,pred,col="red")

#Calculating the Returns
getSymbols("ANIP",src = "yahoo",from=start,to=end)
getSymbols("MSFT",src="yahoo",from=start,to=end)
getSymbols("BAC",src="yahoo",from=start,to=end)
MSFT.return = data.frame(Date = index(MSFT),coredata(MSFT))
ANIP.return = data.frame(Date = index(ANIP),coredata(ANIP))
BAC.return = data.frame(Date = index(BAC),coredata(BAC))
returns_MSFT= ((MSFT.return$MSFT.Close- MSFT.return$MSFT.Open)/MSFT.return$MSFT.Open)*100
returns_ANIP = ((ANIP.return$ANIP.Close- ANIP.return$ANIP.Open)/ANIP.return$ANIP.Open)*100
returns_BAC = ((BAC.return$BAC.Close - BAC.return$BAC.Open)/BAC.return$BAC.Open)*100
returns_df = data.frame(returns_ANIP,returns_BAC,returns_MSFT)
cor_matrix = cor(returns_df)

library(corrplot)
corrplot(cor_matrix,type = "lower",order = "hclust",tl.col = "black",tl.srt = 45)

csv_df = data.frame(allData$Date,allData$ANIP.Adjusted)
write.csv(csv_df,"ANIP.csv")
