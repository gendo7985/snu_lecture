# rm(list=ls())

# install.packages("quantmod")
# install.packages("jsonlite")
library("forecast")

## exercise 1-4-9; X_t = a_20*X_{t-20} + ... + a_1*X_{t-1} + e_t
temperature = read.table("ex_ch1_9.txt"); head(temperature)
temperature = temperature[-1,]
temperature = as.numeric(temperature)
plot(temperature, type = "l")

# design matrix
X = matrix(NA,80,20)
for(i in 1:80){
  X[i,] = temperature[i:(i+19)]
}
Y=temperature[-c(1:20)]

# lm fitting
temp.lm.fit = lm(Y ~ X - 1)
temp.lm.fit$coefficients
arima.sim(model=list(ar=as.numeric(temp.lm.fit$coefficients)), n=1000) # non-stationary

# 10-step ahead prediction
X_t = X[80,]
predicted = c()
for (i in 1:10){
  predicted[i] <- sum(temp.lm.fit$coefficients*X_t)
  X_t <- c(X_t[-1], predicted[i])
}
predicted

# ar fitting
temp.ar.fit <- arima(temperature, order=c(20,0,0), include.mean=F) # non-stationary problem

temp.ar.fit <- arima(temperature, order=c(20,0,0), include.mean=F, method="ML")
pred.ar = forecast(temp.ar.fit, h=10)
pred.ar$mean

## non-invertible MA(1)
set.seed(1)
invma <- arima.sim(model=list(ma=c(0.5)), n=300, start.innov = 0, sd = 1)

set.seed(1)
non_invma <- arima.sim(model=list(ma=c(2)), n=300, start.innov = 0, sd = 0.5)

par(mfrow=c(1,2))
plot(invma, type='l')
plot(non_invma, type='l')
par(mfrow=c(1,1))

# fitting MA(1)
fit_invma <- arima(invma, order=c(0,0,1), include.mean=F)
fit_invma
fit_non_invma <- arima(non_invma, order=c(0,0,1), include.mean=F)
fit_non_invma


## quantmod
library(quantmod)
library(jsonlite)
usd_krw <- getSymbols(Symbols="USD/KRW", 
                      src = "yahoo", 
                      from=  Sys.Date() - 180, 
                      to =  Sys.Date(), auto.assign = FALSE)
plot(usd_krw$USD.KRW)

kospi = getSymbols(Symbols="^KS11", 
                   src = "yahoo", 
                   from= "2021-01-01", 
                   to = Sys.Date(), auto.assign = FALSE)
plot(kospi$KS11.Close)

kosdaq = getSymbols("^kq11",
                    src = "yahoo", 
                    from= Sys.Date() - 1500, 
                    to = Sys.Date(), auto.assign = FALSE)
plot(kosdaq$KQ11.Close)

tesla = getSymbols(Symbols="TSLA",
                   src = "yahoo", 
                   from=  Sys.Date() - 365, 
                   to =  Sys.Date(), auto.assign = FALSE)
plot(tesla$TSLA.Close)

apple = getSymbols(Symbols="AAPL",
                   src = "yahoo", 
                   from=  Sys.Date() - 365, 
                   to =  Sys.Date(), auto.assign = FALSE)
plot(apple$AAPL.Close)

samsung = getSymbols("005930.KS",
                     src = "yahoo", 
                     from= Sys.Date() - 365, 
                     to = Sys.Date(), auto.assign = FALSE)
plot(samsung$`005930.KS.Close`)

jyp = getSymbols("035900.KQ", 
                 src = "yahoo", 
                 from= Sys.Date() - 365, 
                 to = Sys.Date(), auto.assign = FALSE) # error
