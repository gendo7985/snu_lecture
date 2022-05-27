
# rm(list=ls())

# install.packages("forecast")

library("forecast")
?arima        # fit arima model
?forecast     # forecast using fitted model


# MA model example ; MA(1) ; X_{t} = e_{t} + 0.5*e_{t-1}
set.seed(1)
ma_ex <- arima.sim(model=list(ma=c(0.5)), 100)
plot(ma_ex)
abline(h=0, col=grey(0.5), lty=2)


# ARMA model example ; ARMA(2,1) ; X_{t} - 0.5*X_{t-1} - 0.3*X_{t-2} = e_{t} + 0.2*e_{t-1}
set.seed(1)
dat <- arima.sim(model=list(ar=c(0.5, 0.3), ma=c(0.2)), n=200)
plot(dat)

# fitting AR(1)
fit_ar <- arima(dat, order=c(1,0,0), include.mean=F) # include.mean; The default is TRUE for undifferenced series
fit_ar
names(fit_ar)
fit_ar$coef
points(fitted(fit_ar), col=2, type='l')

# fitting MA(1) model
fit_ma <- arima(dat, order=c(0,0,1), include.mean=F)
fit_ma
points(fitted(fit_ma), col=3, type='l')

# fitting ARMA(2,1) model
fit_arma <- arima(dat, order=c(2,0,1), include.mean=F)
fit_arma
points(fitted(fit_arma), col=4, type='l')

# auto.arima
fit_auto <- auto.arima(dat, allowmean=F) # allowmean; If TRUE, models with a non-zero mean are considered.
fit_auto

# RMSE
sqrt(mean(fit_ar$residuals^2))
sqrt(mean(fit_ma$residuals^2))
sqrt(mean(fit_arma$residuals^2))
sqrt(mean(fit_auto$residuals^2))

## prediction 
# h : Number of periods for forecasting
?forecast
pred_arma <- forecast(fit_arma, h=20)
pred_arma
plot(pred_arma)

plot(forecast(fit_auto, h=20), xlim=c(180,220))


## acf and pacf
set.seed(1)
dat_ar <- arima.sim(model=list(ar=c(0.5)), n=500); plot(dat_ar)
dat_ma <- arima.sim(model=list(ma=c(0.5)), n=500); plot(dat_ma)
dat_arma <- arima.sim(model=list(ar=c(0.5), ma=c(0.5)), n=500)


par(mfrow=c(1,2))
acf(dat_ar)
pacf(dat_ar)

result.acf = acf(dat_ar, plot = FALSE)
result.acf$acf[2]
result.pacf = pacf(dat_ar, plot = FALSE)
result.pacf$acf[1]

acf(dat_ma)
pacf(dat_ma)

acf(dat_arma)
pacf(dat_arma)
par(mfrow=c(1,1))
