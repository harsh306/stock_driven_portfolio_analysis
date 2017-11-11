  # Equal Weight Portfolio Weights
  # Amount invested $10,000 in 3 stocks
  # Stock 1= $50 per stock | Stock 2=$100 per stock | Stock 3=$500
  weight=rep(1/3,3)
  weight


  # Market Capitalization Portfolio Weights
  # Outstanding Shares: Stock 1= 100 Stock 2= 100, Stock 3= 100
  mc=c(5000,10000,50000)
  weight_mc=mc/sum(mc)
  weight_mc
  summary(weight_mc)
  barplot(weight_mc,xlab="Stock",ylab="Market Capitalization based weights")


  #Initial Investment (2 Shares each)
  init=c(100,200,1000)
  #Final Value for Period 1
  fin=c(110,210,1200)
  #Simple Return Calculation
  SR1= (sum(fin)-sum(init))/sum(init)
  #Final Value for Period 2
  fin2=c(120,180,900)
  SR2= (sum(fin2)-sum(fin))/sum(fin)
  SR2
  #Returns using weights
  SR_Individual=(fin-init)/init
  PR=weight_mc*SR_Individual
  PR
  sum(PR)


  #Calculating Gross Value of Portfolio given returns in n periods
  gross=init*(1+SR1)*(1-SR2)

  #Calculating Time Series returns on Stocks:
  value=read.zoo(file="values.csv",header=TRUE, format="%m/%d/%Y", sep=",")
  value_ts=as.xts(values)
  value_ts
  asset_return=Return.calculate(value_ts)
  #Remove First row of NA values
  asset_return=asset_return[-1,]

  #Calculating Time Series returns on Portfolio
  #Buy and Hold
  PR_BH=Return.portfolio(R=asset_return, weights=weight_mc)
  #Daily Rebalance
  PR_DR=Return.portfolio(R=asstet_return, weights=weight_mc, rebalance_on="days")
  par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
  plot.zoo(PR_BH)
  plot.zoo(PR_DR)

  #Reducing frequency from Daily close to monthly close values. 
  # Eg. Convert a stocks end of day prices (Test2) and convert it to monthly values (Works on Univariate distribution)

  spd=read.zoo(file="Test2.csv",header=TRUE, format="%m/%d/%Y", sep=",")
  spd_ts=as.xts(spd)
  spm=to.monthly(spd_ts)
  spm
  spm_returns=Return.calculate(spm[,4])
  plot.zoo(spm_returns)
  table.CalendarReturns(spm_returns)

  #Computing performance evaluation parameters

  mean(spm_returns[-1,])
  mean.geometric(spm_returns)
  sd(spm_returns[-1,])

  #Evaluating Risk vs Reward on the portfolio (Comparison with a benchmark-US Treasury Bill)

  rfm=read.zoo(file="rftest.csv",header=TRUE, format="%m/%d/%Y", sep=",")
  rf=as.xts(rfm)
  sp_excess =spm_returns -rf
  mean(sp_excess)
  # Compute the Sharpe ratio
  sp_sharpe = mean(sp500_excess) / sd(spm_returns[-1,])

  #  Converting monthly performance measures to annualized measures to show performance over an annual investment horizon
  #  Compute the annualized mean
  am=Return.annualized(spm_returns,scale=12)

  #  Compute the annualized standard deviation
  as=StdDev.annualized(spm_returns,scale=12)

  #  Compute the annualized Sharpe ratio without risk free rate into consideration: ann_sharpe
  ann_sharpe = Return.annualized(spm_returns,scale=12) / StdDev.annualized(spm_returns,scale=12)

  # Understanding Sharpe Ratio interpretation:
https://www.investopedia.com/ask/answers/010815/what-good-sharpe-ratio.asp

  #  Compute all of the above at once using table.AnnualizedReturns()
  table.AnnualizedReturns(spm_returns,scale=12)

  # Calculating Sharpe Ratio with the Risk Free rate in consideration
  sharpe_ann= SharpeRatio.annualized(sp500_returns, Rf = rf)

  #  Plotting the 12-month rolling annualized mean
  chart.RollingPerformance(R = spm_returns, width = 12, FUN = "Return.annualized")
  abline(h = am)

  #  Plotting the 12-month rolling annualized standard deviation
  chart.RollingPerformance(R = spm_returns, width = 12,  FUN = "StdDev.annualized")
  abline(h = as)

  #  Plotting the 12-month rolling annualized Sharpe ratio
  chart.RollingPerformance(R = spm_returns, width = 12, FUN = "SharpeRatio.annualized", Rf = rf)
  abline(h = sharpe_ann)


  # Subperiod Performance Analysis using Window function
  #  Fill in window for, eg. 2008
  spm_2008 =window(spm_returns, start  = "2008-01-01", end = "2008-12-31")

  #  Create window for, eg. 2014
  spm_2014 = window(spm_returns, start = "2014-01-01", end = "2014-12-31")

  #  Plotting settings 
  par( mfrow = c(1, 2) , mar=c(3, 2, 2, 2))
  names(spm_2008)= "spm_2008"
  names(spm_2014)= "spm_2014"

  #  Plot histogram of 2008
  chart.Histogram(spm_2008, methods = c("add.density", "add.normal"))

  #  Plot histogram of 2014
  chart.Histogram(spm_2014, methods = c("add.density", "add.normal"))

  # Detecting Non-Normality of Returns using Skewness and Kurtosis
  #  Compute the skewness
  skewness(spm_returns)
  kurtosis(spm_returns)

  # Downside Risk Measures
  #  Understanding more about Downside Risk:
  https://www.risk.net/risk-magazine/technical-paper/1506669/var-versus-expected-shortfall

  #  Calculate the SemiDeviation
  SemiDeviation(spm_returns)

  #  Calculate the value at risk
  VaR(spm_returns,p=0.025)
  VaR(spm_returns, p=0.05)

  #  Calculate the expected shortfall
  ES(spm_returns, p=0.025)
  ES(spm_returns, p=0.05)


  # Visualizing Drawdowns
  #  Table of drawdowns
  table.Drawdowns(spm_returns)

  #  Plot of drawdowns
  chart.Drawdown(spm_returns)


