# rm(list=ls())

?rnorm      # sampling from normal distribution
?cumsum     # cumulative sum
?arima.sim  # sampling from arima model
?abline     # add a straight line to the plot


## random walk; X_{t} = X_{t-1} + e_t
epsilon <- rnorm(n=100, mean=0, sd=1)
rand.walk <- cumsum(epsilon)
plot(rand.walk, type='l',
     xlab="Time", ylab="X", main="Time series plot of Random walk")
set.seed(1)
plot(cumsum(rnorm(100)), type='l',
     xlab="Time", ylab="X", main="Time series plot of Random walk")

## AR model example
# ar_ex ; AR(1) ; X_{t} - 0.5*X_{t-1} = e_{t}
set.seed(1)
ar_ex <- arima.sim(model=list(ar=c(0.5)), n = 100)
plot(ar_ex)
abline(h=0, col=grey(0.5), lty=2)

# burn-in
set.seed(1)
ar_ex_burn_in <- arima.sim(model=list(ar=c(0.5)), n = 100, n.start=30)
plot(ar_ex_burn_in)
abline(h=0, col=grey(0.5), lty=5)

set.seed(1)
x=15
eps <- rnorm(n=130, mean=0, sd=1)
for (i in 2:130) {
  x[i] = 0.5*x[i-1] + eps[i]
}
plot(x, type = "l")
plot(x[-c(1:30)], type = "l")


# innovation option
set.seed(1)
ar_ex <- arima.sim(model=list(ar=c(0.5)), n = 100, rand.gen = function(n) rt(n, df = 5))
plot(ar_ex)
abline(h=0, col=grey(0.5), lty=2)


# ar_ex2; AR(2) ; X_{t} - 0.5*X_{t-1} - 0.3*X_{t-2} = e_{t}
ar_ex2 <- arima.sim(model=list(order=c(2,0,0), ar=c(0.5, 0.3)), n = 100)
plot(ar_ex2)
abline(h=0, col=grey(0.5), lty=5)


## read csv file

library(lubridate)
dir <- choose.dir()
setwd(dir) # setwd("C:\\Users\\kdwok\\OneDrive - SNU\\시계열분석 및 실습\\2022-1\\실습자료\\Data set")
getwd()
data <- read.csv("kospi2.csv")
fix(data)

kospi <- data[,1:2]
tail(kospi)

kospi[,1] <- ymd(kospi[,1])
tail(kospi)

kospi[,2] <- gsub(",", "", kospi[,2])
tail(kospi)
plot(kospi, type='l', xlab="Time", ylab="KOSPI", main="Time series plot of KOSPI")


# smoothing

kospi[1,2]+kospi[2,2] # non-numeric argument
typeof(kospi[,2])
kospi[,2] <- as.numeric(kospi[,2])

kospi_smooth <- c()
m=100
for(i in (m+1):length(kospi[,2])) {
  kospi_smooth[i] <- mean(kospi[(i-m):i,2])
}
lines(kospi[,1], kospi_smooth, col=2)


