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
k = 10
i = sample(rep(1:k),length.out = newData)
cv_tmp = matrix(NA,nrow = k,ncol = length(newData))
for (j in i:k){
  test_j = which(i == j)
  train = xy[-test_j,]
  test = xy[test_j,]
  x = train
  y = test
  model = lm(ANIP.Adjusted~.,data=x)
  pred_cv = predict(model,y)
  acc = accuracy(pred_cv,y$ANIP.Adjusted)
}