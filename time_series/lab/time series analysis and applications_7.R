# rm(list=ls())

# install.packages("tseries")
library(tseries)
library(forecast)
library(quantmod)

## Gaussian random walk
t <- c(0:100)
eps <- rnorm(100, 0, 1)
x <- c(0, cumsum(eps))
plot(t, x, type = "l", ylim = c(-25, 25))

random_walk = function(N, n){
  e=matrix(rnorm(N*n), nrow=N, ncol=n)
  return(t(apply(e, 1, cumsum)))
}

RW = random_walk(100, 100)
X <- cbind(rep(0, 100), RW)

plot(t, X[1,], ylim = c(-25, 25), type = "l")
apply(X[2:100, ], 1, function(x, t) lines(t, x), t = t)
lines(t, sqrt(t)*1.96, col=2); lines(t, -sqrt(t)*1.96, col=2)

plot(t, X[1,], xlim = c(0, 50), ylim = c(-18, 18), type = "l")
apply(X[2:100, ], 1, function(x, t) lines(t, x), t = t)
lines(t, sqrt(t)*1.96, col=2); lines(t, -sqrt(t)*1.96, col=2)


## approximation of Brownian motion
Wns = function(mat, s){
  n=ncol(mat)
  ns=floor(n*s)
  if (ns < 1) {
    N=nrow(mat)
    return(rep(0, N))
  }
  return(mat[, ns]/sqrt(n))
}

RW = random_walk(10000, 10000)

t1=0.1; t2=0.3; t3=0.9
Wt1 = Wns(RW, t1)
Wt2 = Wns(RW, t2)
Wt3 = Wns(RW, t3)

Wns(RW, 0)
mean(Wt3)
var(Wt3)

mean(Wt3-Wt2)
var(Wt3-Wt2)
cor(Wt3-Wt2, Wt2-Wt1)
cor(Wt2-Wt1, Wt1)

## normality test
# Jarque-Beta test
jarque.bera.test(Wt3-Wt2)

# Shapiro-Wilk test
shapiro.test((Wt3-Wt2)[1:5000])

# Kolmogorov-Smirnov test
ks.test(Wt3-Wt2, rnorm(seq(-3,3,length=1000), mean(Wt3-Wt2), sd(Wt3-Wt2)))
ks.test(Wt3-Wt2, "pnorm", mean(Wt3-Wt2), sd(Wt3-Wt2))


### unit root test, H0 : phi = 1
?adf.test

## iid vs RW
x <- rnorm(1000)  # no unit-root
adf.test(x)

y <- diffinv(x)   # contains a unit-root
adf.test(y)

## ARIMA(1,1,1)
set.seed(1)
dara_arima111 <- arima.sim(model=list(order=c(1,1,1), ar=0.2, ma=0.3), n=1000)

adf.test(dara_arima111) # arima(1,1,1)

adf.test(diff(dara_arima111))


## non-stationary AR(1); X_t = phi*X_{t-1} + e_t
phi = -1 # phi = 1.1

set.seed(1)
dara_arima1113 = 0
for(i in 2:1000){
  dara_arima1113[i] = phi*dara_arima1113[i-1] + rnorm(1)
}
plot(dara_arima1113)

adf.test(dara_arima1113) # phi<1, but non-stationary

# explosive test, H1 : phi > 1
adf.test(dara_arima1113, alternative = "explosive")


## convergence rate & sationarity
set.seed(1)
data_ar1_n100 <- arima.sim(model=list(order=c(1,0,0), ar=0.9), n=100)
data_ar1_n1000 <- arima.sim(model=list(order=c(1,0,0), ar=0.9), n=1000)

adf.test(data_ar1_n100)

adf.test(data_ar1_n1000)


### model identification

## acf and pacf
set.seed(1)
data_ar2 = arima.sim(model=list(order=c(2,0,0), ar=c(0.2, 0.5)), n=1000)
data_ma2 = arima.sim(model=list(order=c(0,0,2), ma=c(0.4, 0.3)), n=1000)
data_arma21 = arima.sim(model=list(order=c(2,0,1), ar=c(0.2, 0.5), ma=0.3), n=1000)

acf(data_ar2)
pacf(data_ar2)

acf(data_ma2)
pacf(data_ma2)

acf(data_arma21)
pacf(data_arma21)

## aicc, aic, bic
set.seed(1)
dara_arima111 = arima.sim(model=list(order=c(1,1,1), ar=0.5, ma=0.3), n=1000)
plot(dara_arima111)

fit_ar1 <- Arima(dara_arima111, order=c(1,0,0))
fit_ma1 <- Arima(dara_arima111, order=c(0,0,1))
fit_arima111 <- Arima(dara_arima111, order=c(1,1,1))
fit_auto <- auto.arima(dara_arima111, allowdrift = F)
# phi(B)*(X_t-b*t-c) = theta(B)*e_t
# phi(B)*(DX_t-b) = theta(B)*e_t, DX_t=X_t-X_{t-1}, b:drift

fit_ar1$aic
fit_ma1$aic
fit_arima111$aic
fit_auto$aic

### model diagnostics
res_ma1 <- fit_ma1$residuals
res_arima111 <- fit_arima111$residuals

## Graphical methods
mean(res_ma1)
acf(res_ma1)
plot(res_ma1, type='l')

hist(res_ma1)
qqnorm(res_ma1)
qqline(res_ma1, col = "steelblue", lwd = 2)

mean(res_arima111)
plot(res_arima111, type='l')
hist(res_arima111)
qqnorm(res_arima111)
qqline(res_arima111, col = "steelblue", lwd = 2)
acf(res_arima111)

## autocorrelation of residuals
# Ljung-Box test
Box.test(res_ma1, type="Ljung-Box")
Box.test(res_arima111, type="Ljung-Box")
Box.test(res_ma1) # Box-Pierce
Box.test(res_arima111)

# Durbin-Watson test
d_ma1 = sum((res_ma1 - Lag(res_ma1))^2, na.rm = TRUE) / sum(res_ma1^2, na.rm = TRUE); d_ma1
d_arima111 = sum((res_arima111 - Lag(res_arima111))^2, na.rm = TRUE) / 
  sum(res_arima111^2, na.rm = TRUE); d_arima111

## normality test
# Jarque-Beta test
jarque.bera.test(res_ma1)
jarque.bera.test(res_arima111)

# Shapiro-Wilk test
shapiro.test(res_ma1)
shapiro.test(res_arima111)

# Kolmogorov-Smirnov test
ks.test(res_ma1, rnorm(seq(-3,3,length=1000), mean(res_ma1), sd(res_ma1)))
ks.test(res_ma1, "pnorm", mean(res_ma1), sd(res_ma1))

ks.test(res_arima111, rnorm(seq(-3,3,length=1000), mean(res_arima111), sd(res_arima111)))
ks.test(res_arima111, "pnorm", mean(res_arima111), sd(res_arima111))
