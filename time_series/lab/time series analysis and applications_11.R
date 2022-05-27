
# install.packages("aTSA")
library(aTSA)
library(fGarch)
library(forecast)

### ARCH Engle's Test For Residual Heteroscedasticity(Portmanteau Q)
set.seed(1)
data_normal <- rnorm(1000)
fit <- arima(data_normal, order = c(0,0,0))
arch.test(fit)

set.seed(1)
data_arma <- arima.sim(model = list(ar=-0.3, ma=0.5), n=1000)
fit_arma <- arima(data_arma, order = c(1,0,1))
arch.test(fit_arma)

spec_garch <- garchSpec(model = list(omega=1, alpha=0.3, beta=0.4), rseed=1)
data_garch <- garchSim(spec = spec_garch, n=1000)
fit_arima <- auto.arima(data_normal); fit_arima
fit2 <- arima(data_garch, order = c(0,0,0))
arch.test(fit2)

?arch.test
Box.test(fit2$residuals^2, type = "Ljung-Box", lag=20)

### change point detection
# autocovariance function
cov_calc <- function(dat, h) {
  n <- length(dat)
  xmean <- mean(dat)
  x1 <- dat[1:(n-h)] - xmean
  x2 <- dat[(h+1):n] - xmean
  return(sum(x1 * x2/n))
}

# long run variance
var_calc <- function(dat, max_h = 2^0.5*(log10(length(dat)))^2 ) {
  n <- length(dat)
  sd2_hat <- cov_calc(dat, 0)
  for(i in 1:max_h) {
    sd2_hat <- sd2_hat + 2*(1-i/n)*(cov_calc(dat, i)) # Bartlett kernel
  }
  return(sd2_hat)
}

CUSUM_calc <- function(dat) {
  ## return : maximum cusum test statistics and change point
  n <- length(dat)
  cusum <- abs((cumsum(dat) - (1:n)/n*sum(dat) ) / ( sqrt(n) * sqrt(var_calc(dat))))
  argmax <- which.max(cusum)
  if(max(cusum)>1.358) return(list("CUSUM_statistics" = max(cusum), "change_point"=argmax))
  else return(print("no change"))
}


## mean change
set.seed(1)
CUSUM_calc(c(rnorm(500,0,1), rnorm(500,1,1)))
CUSUM_calc(c(rnorm(500,0,1), rnorm(500,0,1))) ## no change points


## parameter change in garch(1,1)
# no change
spec_garch <- garchSpec(model = list(omega=1, alpha=0.3, beta=0.4), rseed=1)
data_garch <- garchSim(spec = spec_garch, n=1000)
plot(data_garch)

fit_garch <- garchFit(formula = ~ garch(1,1), data = data_garch, trace=F)
xi_t = residuals(fit_garch, standardize=T)
xi_t - (data_garch-fit_garch@fit$par[1])/fit_garch@sigma.t

plot(xi_t, type='l')
CUSUM_calc((xi_t)^2)

# change at t = 501; omega/(1-alpha-beta)
spec_change1 <- garchSpec(model = list(omega=1, alpha=0.3, beta=0.1), rseed=1)
spec_change2 <- garchSpec(model = list(omega=1, alpha=0.7, beta=0.2), rseed=1)
data_change1 <- garchSim(spec=spec_change1, n=500)
data_change2 <- garchSim(spec=spec_change2, n=500)
data_change <- c(data_change1, data_change2)
plot(data_change, type='l')

fit_change <- garchFit(formula = ~ garch(1,1), data = data_change, trace=F)
xi_t = residuals(fit_change, standardize=T)
plot(xi_t, type='l')
changepoint <- CUSUM_calc((xi_t)^2); changepoint
abline(v=501, col=2) ## true change point
abline(v=changepoint$change_point, col=3)

# change at t = 901
spec_change1 <- garchSpec(model = list(omega=1, alpha=0.3, beta=0.1), rseed=1)
spec_change2 <- garchSpec(model = list(omega=1, alpha=0.7, beta=0.2), rseed=1)
data_change1 <- garchSim(spec=spec_change1, n=900)
data_change2 <- garchSim(spec=spec_change2, n=100)
data_change <- c(data_change1, data_change2)
plot(data_change, type='l')

fit_change <- garchFit(formula = ~ garch(1,1), data = data_change, trace=F)
xi_t = residuals(fit_change, standardize=T)
plot(xi_t, type='l')
changepoint <- CUSUM_calc((xi_t)^2); changepoint

