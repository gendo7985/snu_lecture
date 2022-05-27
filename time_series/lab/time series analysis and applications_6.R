library(forecast)

n=1000
set.seed(1)
ts_AR <- arima.sim(n = n, list(ar = 0.5,ma=0.6))
f=arima(ts_AR,order=c(1,0,1), include.mean=FALSE)
phi_ml = as.numeric(f$coef[1])
theta_ml = as.numeric(f$coef[2])

e=rep(0, 1000)
for(i in 2:1000){
  e[i] = ts_AR[i] - phi_ml*ts_AR[i-1] - theta_ml*e[i-1]
}

f$residuals[1:10]
e[1:10]

f$residuals[11:20]
e[11:20]

tail(f$residuals)
tail(e)


## generating residuals using initial value (CSS)
f_css=arima(ts_AR,order=c(1,0,1), include.mean=FALSE, method = "CSS")
phi_css = as.numeric(f_css$coef[1])
theta_css = as.numeric(f_css$coef[2])
residual = c()
residual[1] = 0
for(i in 2:1000){
  residual[i] = ts_AR[i]-phi_css*ts_AR[i-1]-theta_css*residual[i-1]
}

f_css$residuals[1:10]
residual[1:10]


## Backcasting; X_{t-1}-phi_ml*X_t = epsilon_{t-1} + theta_ml*epsilon_t
g=arima(ts_AR[1000:1],order=c(1,0,1), include.mean=FALSE)
f; g

m=100
residual = c()
residual[1] = 0
for(i in 2:1000){
  residual[i] = ts_AR[i]-phi_ml*ts_AR[i-1]-theta_ml*residual[i-1]
}
for (i in (m:2)){
  residual[i-1] = ts_AR[i-1]-phi_ml*ts_AR[i]-theta_ml*residual[i] # Backasting
}
for(i in 2:1000){
  residual[i] = ts_AR[i]-phi_ml*ts_AR[i-1]-theta_ml*residual[i-1]
}

f$residuals[1:10]-residual[1:10]

f$residuals[11:20]-residual[11:20]

tail(f$residuals)-tail(residual)


## Kalman filter
SSM <- makeARIMA(phi = phi_ml, theta = theta_ml, Delta = numeric(0))
kf <- KalmanRun(ts_AR, mod = SSM)
sum(f$residuals == kf$resid)
