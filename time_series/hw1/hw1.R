# 7
set.seed(0)
eps <- rnorm(99)
X <- c(0)
for (i in 1:99) {
  X[i+1] <- 0.3*X[i] + eps[i]
}
plot(1:100, X, type='b', main="X_t")
Y <- X + 0:99
Z <- X + sin(0:99)
plot(1:100, Y, type='b', main="Y_t")
plot(1:100, Z, type='b', main="Z_t")

# 8
kospi <- read.csv('ex_ch1_8.txt')
plot(1:100, kospi$data, type="b", main="kospi", xlab='2004.01 ~ 2012.04')
ma <- function(m, data){
  x <- (m+1):length(data)
  y <- c()
  for (i in 1:length(x)) {
    y[i] <- sum(data[i:(i+m)]) / (m+1)
  }
  return(data.frame(x, y))
}

lines(ma(5, kospi$data), col='blue')
lines(ma(7, kospi$data), col='red')
lines(ma(10, kospi$data), col='green')
lines(ma(15, kospi$data), col="orange")
lines(ma(20, kospi$data), col='yellow3')
legend(0, 2150, legend=c('m=5', 'm=7', 'm=10', 'm=15', 'm=20'),
       fill=c('blue','red','green','orange','yellow3'))

# 10
temperature <- read.csv('ex_ch1_9.txt')
plot(temperature$data, xlim=c(0,110))

## construct data.frame
data <- cbind(temperature$data[21:100], temperature$data[20:99])
for (i in 2:20) {
  data <- cbind(data, temperature$data[(21-i):(100-i)])
}
colnames(data) <- c('Xt', paste0(rep('X_t-', 20), as.character(1:20)))
temp <- as.data.frame(data)

## fit a1, ..., a20
fit <- lm(Xt ~ . - 1, data=temp)
summary(fit)
fit$coefficients

## predict X_101 ~ 110
temp <- temperature$data
for (i in 101:110) {
  temp[i] <- sum(temp[(i-1):(i-20)] * fit$coefficients)
}
plot(temp[1:100], type='b', xlim=c(0,110), ylim=c(18,32),
     main='Daily Temperature', xlab='2012.05.28 ~ 09.04', ylab='temperature')
lines(101:110,temp[101:110], type='b', col='red')
temp[101:110]
