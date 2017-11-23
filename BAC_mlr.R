library(quantmod);library(tseries);
library(timeSeries);library(forecast);library(xts);
library(TTR)
library ( gam )

start = as.Date("2013-01-01")  
end = as.Date("2016-01-31")

#get data for BAC
getSymbols("BAC",src = "yahoo",from=start,to=end)
BAC.df = data.frame(Date = index(BAC),coredata(BAC))

# Apply Transformations as TA indicators
sma10 <- SMA(BAC.df[7],n = 10)
BAC.df['SMA'] <- sma10

rsi10 <- RSI(BAC.df[7],n=10)
BAC.df['RST'] <- rsi10

cci10 <- CCI(HLC = BAC.df[3:5],n = 10)
BAC.df['CCI'] <- cci10

bbands10 <- BBands(HLC = BAC.df[3:5],n=10)
BAC.df['BBands'] <- bbands10

adx10 <- ADX(HLC = BAC.df[3:5],n=10)
BAC.df['ADX'] <- adx10

a <- BAC.df[2:length(BAC.df$BAC.Adjusted),]$BAC.Adjusted
BAC.df['BAC.predict'] <- c(a,BAC.df[length(BAC.df$BAC.Adjusted),7])

#omitting NA
BAC.filter1 <- na.omit(BAC.df)
c(1:length(BAC.df))
BAC.filter1[13]

#normalize dataset
for(i in c(2:length(BAC.df))){
    BAC.filter1[i] <- scale(BAC.filter1[i])
}

#saving the data set
save(BAC.filter1,file = "/home/harsh/ml/stocks/portfolio/Noramalized_BAC_MLR.Rda")  

#SLR
fit_slr <- lm(BAC.filter1$BAC.predict~BAC.filter1$BAC.Adjusted, data = BAC.filter1)
summary.lm(fit_slr)
plot(BAC.filter1$Date,BAC.filter1$BAC.predict,las=1,pch=20,col = "grey")
lines(BAC.filter1$Date,fitted(fit_slr),lwd = 3,col="red")

#MLR
fit_mlr <- lm(BAC.filter1$BAC.predict~., data = BAC.filter1)
summary.lm(fit_mlr)
plot(BAC.filter1$Date,BAC.filter1$BAC.predict,las=1,pch=20,col = "grey")
lines(BAC.filter1$Date,fitted(fit_mlr),lwd = 3,col="red")

#Splines SLR
fit_spline <- smooth.spline(BAC.filter1$BAC.Adjusted , BAC.filter1$BAC.predict)
summary(fit_spline)
fit_spline
plot(BAC.filter1$Date, BAC.filter1$BAC.predict,col="grey",xlab="",ylab="y_norm")
lines(BAC.filter1$Date,fitted(fit_spline),col="red",lwd=2)

#GAM spline mlr
attach(BAC.filter1)
#fit_gam <- gam(BAC.filter1$BAC.predict ~ s(BAC.filter1$BAC.Open,4)+s(BAC.filter1$BAC.High,4)+s(BAC.filter1$BAC.Low,4)+s(BAC.filter1$BAC.Volume,4)+s(BAC.filter1$BAC.Adjusted,4)+s(BAC.filter1$SMA,4)+s(BAC.filter1$RST,4)+s(BAC.filter1$CCI,4))
fit_gam <- gam(BAC.filter1$BAC.predict ~. , data = BAC.filter1)
summary.gam(fit_gam)
par(mfrow=c(1,12))
#plot(fit_gam,se = TRUE, pages= 12,all.terms=TRUE,scheme=1)
