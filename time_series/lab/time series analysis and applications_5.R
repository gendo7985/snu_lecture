# rm(list=ls())
library(forecast)
library(quantmod)
library(jsonlite)

### Variance Stabilizing Transformation
## power transformation; (y_t)^a
## Box-Cox transformation; {(y_t)^a - 1}/a, a!=0
## log transformation; log(y_t), a==0

### diff
## difference; y_t-y_{t-1} = (y_t-y_{t-1})/(t-(t-1))
## return; (y_t-y_{t-1})/y_{t-1} = (y_t / y_{t-1}) - 1
## logarithmic return; log(y_t)-log(y_{t-1}) = log(y_t / y_{t-1}) ~ return, when (y_t / y_{t-1}) ~ 1

(200-100); (50-100)
(200-100)/100; (50-100)/100
log(200)-log(100); log(50)-log(100)

### example
x = 50:200
plot(x, (x-100)/100, type='l') # return
points(x, log(x)-log(100), col=3, type='l') # logarithmic return

### apple stock price
apple = getSymbols(Symbols="AAPL",
                   src = "yahoo", 
                   from=  "2010-01-01", 
                   to =  Sys.Date(), auto.assign = FALSE)
plot(apple$AAPL.Close)
apple_close = as.numeric(apple$AAPL.Close)
n=length(apple_close)

## Variance Stabilizing Transformation
boxcox.y_t = BoxCox(apple_close, 0.1)
log.y_t = log(apple_close)

plot(boxcox.y_t, type="l")
plot(log.y_t, type="l")

## diff
difference.y_t = diff(apple_close)
return.y_t = (difference.y_t)/apple_close[-n]
log_return.y_t = diff(log(apple_close))

plot(difference.y_t, type="l")
plot(return.y_t, type="l")
plot(log_return.y_t, type="l")

##### ARIMA simulation #####

### data generating; ARIMA(1,1,1)
set.seed(11)
arma_sample <- arima.sim(model=list(ar=c(-0.2), ma=c(0.3)), n=300)
plot(arma_sample)

arima_sample <- cumsum(arma_sample) # integrated
plot(arima_sample, type='l')

set.seed(11)
arima_sample2 <- arima.sim(model=list(order=c(1,1,1), ar=c(-0.2), ma=c(0.3)), n=300)
points(arima_sample2, col=2, type='l')


## data fitting
data <- arima_sample2
traindata <- data[1:200]
arima.fit <- arima(traindata, order=c(1,1,1))

### data forecast

## 100-step ahead forecast
arima.forecast <- forecast(arima.fit, h=100)

arima.fit
plot(arima.forecast, col=2)
points(data, type='l')

arima_RMSE1=sqrt(mean((data[201:300]-arima.forecast$mean)^2)); arima_RMSE1

## one step ahead forecast using new observations without re-estimation
arima.forecast2 <- Arima(data[201:300], model=arima.fit)
arima.forecast2$fitted

arima_RMSE2=sqrt(mean(arima.forecast2$residuals^2)); arima_RMSE2
accuracy(arima.forecast2)

## one step ahead forecast using new observations with re-estimation
prediction=c()
for(i in 1:100){
  data2 = data[i:(199+i)]
  arima.fitting = arima(data2, order=c(1,1,1))
  prediction[i] = forecast(arima.fitting, h=1)$mean[1]
}
residual = data[201:300] - prediction

# out-of-sample test error
arima_RMSE3=sqrt(mean(residual^2)); arima_RMSE3

## comparison plot
plot(arima.forecast)
points(data, type='l')
points(201:300, fitted(arima.forecast2), col=2, type='l')
points(201:300, prediction, col=3, type='l')
