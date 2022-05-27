# rm(list=ls())

# install.packages("sarima")

library(sarima)
library(forecast)
library(parallel)

## sampling
?sim_sarima

# ar(1)
set.seed(1)
ar1 <- list(ar=0.8)
data_ar1 <- sim_sarima(n=100, model=ar1, n.start=30)
plot(data_ar1, type='l')

# arima(1,1,1)
set.seed(1)
arima111 <- list(ar=0.8, iorder=1, ma=0.3) 
data_arima111 <- sim_sarima(n=100, model=arima111, n.start=30)
plot(data_arima111, type='l')

# sarima(0,0,0)(1,0,0)[12]
set.seed(1)
sarima000100_12 <- list(sar=0.8, nseasons=12)
data_sarima000100_12 <- sim_sarima(n=100, model=sarima000100_12, n.start=30)
plot(data_sarima000100_12, type='l')

# sarima(0,0,0)(0,0,1)[12]
set.seed(1)
sarima000001_12 <- list(sma=0.8, nseasons=12) 
data_sarima000001_12 <- sim_sarima(n=100, model=sarima000001_12, n.start=30)
plot(data_sarima000001_12, type='l')

# sarima(1,0,0)(1,1,0)[12]
set.seed(1)
sarima100110_12 <- list(ar=0.3, sar=0.4, siorder=1, nseasons=12)
data_sarima100110_12 <- sim_sarima(n=100, model=sarima100110_12, n.start=30)
plot(data_sarima100110_12, type='l')


# sarima(1,0,0)(1,1,0)[12]
set.seed(1)
data_sample <- sim_sarima(n=500, model=sarima100110_12, xintercept=0.3, n.start=30)
data_sample_ts <- ts(data_sample, frequency=12)
plot(data_sample_ts, type='l')

findfrequency(data_sample) # wrong
findfrequency(data_sample-ma(data_sample, 10)) # wrong
findfrequency(diff(data_sample)) # wrong

acf(data_sample)
acf(diff(data_sample))
lag.plot(data_sample, set = c(1:16), pch = ".", col = "gold")
lag.plot(diff(data_sample), set = c(1:16), pch = ".", col = "gold")

## fitting
fit_sarima <- Arima(data_sample, order=c(1,0,0),
                    seasonal=list(order=c(1,1,0), period = 12), include.drift=T)
fit_sarima
fit_sarima$arma # order = c(ar, ma, sar, sma, period, iorder, siorder)

fit_sarima2 <- Arima(data_sample ,order=c(1,0,0), 
                     seasonal=list(order=c(1,1,0)), include.drift=T) # which uses period=frequency()
fit_sarima2

fit_sarima3 <- Arima(data_sample_ts ,order=c(1,0,0),
                     seasonal=list(order=c(1,1,0)), include.drift=T)
fit_sarima3 # same as fit_sarima
fit_sarima3$residuals

forecast(fit_sarima3)
plot(forecast(fit_sarima3))

## auto.arima
auto.fit_sarima1 <- auto.arima(data_sample)
auto.fit_sarima1

?auto.arima

auto.fit_sarima2 <- auto.arima(data_sample_ts) # stepwise=TRUE
auto.fit_sarima2

detectCores()

auto.fit_sarima3 <- auto.arima(data_sample_ts, parallel=TRUE, stepwise=FALSE, 
                               num.cores=6, max.order=8)
auto.fit_sarima3

## real data example
z <- scan("food.txt")
z_ts = ts(z, start=c(1981,1), frequency=12)
fit11 <- auto.arima(z_ts); fit11
fit12 <- auto.arima(z_ts, parallel=TRUE, stepwise=FALSE, 
                    num.cores=6, max.order=8); fit12 # AIC=722.41   AICc=722.73

# ARIMA(0,1,1)(0,1,1)[12]
Arima(z_ts ,order=c(0,1,1),
      seasonal=list(order=c(0,1,1)), include.drift=F)


# https://archive.ics.uci.edu/ml/datasets.html
# https://fred.stlouisfed.org/
# https://finance.yahoo.com/
# https://www.kaggle.com/
# https://www.data.gov/
# https://catalog.data.gov/dataset/crime-data-from-2010-to-present
# http://data.europa.eu/euodp/en/data
# http://ec.europa.eu/eurostat/data/database
# https://www.google.com/publicdata/directory
# http://data.imf.org/?sk=388DFA60-1D26-4ADE-B505-A05A558D9A42&sId=1479329132316
