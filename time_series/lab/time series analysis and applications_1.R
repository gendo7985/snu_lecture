

print("hello, world!")

1+1
a <- 2; b = 3
a+b
c <- a+b; c; print(c)



# list objects
ls()

# Remove Objects
rm(a)
rm(list=ls())

# help
?rm
help(rm)


##### data types #####

## vector
a <- c(1, 2, 5.3, 6, -2,4) # numeric vector
b <- c("one", "two", "three") # character vector
c <- c(TRUE, TRUE, FALSE, TRUE, FALSE) # logical vector
a[1]; b[2]; c[3]
a[-1]


## data frame
d <- 1:4
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
testdata <- data.frame(d,e,f)
names(testdata) <- c("ID","Color","Passed") # variable names 
testdata
testdata[1,]
testdata[,1]
testdata[,c(1,3)]


## matrix
g <- matrix(c(0:3),2,2); g
?matrix
g1 <- matrix(c(0,1,2,3,4,5), 2, 3); g1
g2 <- matrix(c(2,3,4,5,0,1), 3, 2); g2
m1 <- matrix(c(2,3,4,5,0,1), 3, 2, byrow=T); m1
dim(g1)

g1 %*% g2 # matrix multiplication
g2 %*% g1
t(g1)*g2 # component wise multiplication

## list variable
h <- list(a, e, g); h
h[[1]]
h[[2]][1]
h[[3]][1,2]

##### functions #####
a
mean(a); median(a)
var(a); sd(a)
summary(a)
boxplot(a)
which.max(c(11:20)) # which.max() identifies the index of the largest element of a vector
which(c(11:20)!=20)

# rnorm : sample from normal distribution
?rnorm
x1 <- rnorm(100, 0, 1)
y1 <- 1.5*x1+rnorm(100, 0, 1)
mean(x1)
var(x1)

# dnorm ; pdf value of normal distribution at ()
dnorm(0)

# pnorm : cdf value of normal distribution at ()
pnorm(0)

# qnorm : quantile value of normal distribution at ()
qnorm(0.5)


##### user-defined function #####

temp_function1 <- function(a, b) {
  temp1 <- a-b
  return(temp1)
}
test1 <- temp_function1(2, 3)
test1

temp_function2 <- function(a, b) {
  temp2_1 <- a+b
  temp2_2 <- a-b
  temp2 <- list(temp2_1, temp2_2)
  return(temp2)
}
test2 <- temp_function2(1, 2)
test2
test2[[1]]
test2[[2]]


##### loop #####

test3 <- 0; test3
test4 <- c(); test4 # NULL
for(i in 1:10) {
  test3 <- test3 + i
  test4[i+1] <- i
}
test3
test4


##### plots #####

stem(x1)
par(mfrow=c(1,2))
hist(x1)
plot(x1)
par(mfrow=c(1,1))
plot(x1, type='l')
plot(1:20, 1:20, pch = 1:20)     # plotting with symbols; "+", ...
legend("topleft", legend=c("A", "B"), col=c("red","blue"), lty=1, lwd=2, cex=.8)
dev.off()

regfit <- lm(y1 ~ x1) # linear regression
plot(x1, y1)
abline(regfit, col=2)


##### data analysis #####

# https://cran.rstudio.com/bin/windows/Rtools/
.libPaths() # library paths

# install.packages("lubridate")
library(lubridate)

# Set Working Directory
getwd()
setwd("C:\\Users\\kdwok\\OneDrive - SNU\\시계열분석 및 실습\\2022-1\\실습자료\\Data set")

# read a csv file
kospi <- read.csv("kospi.csv")
head(kospi)
fix(kospi)
tail(kospi, 3)

kospi[,1] <- mdy(kospi[,1]); head(kospi); tail(kospi) # mdy
kospi2 <- kospi[,c(1,2)]; head(kospi2)


a <- which(diff(as.numeric(substr(kospi2[,1], 6, 7)))!=0)
head(a)

kospi_mean <- c()
for(i in 1:length(a)) {
  if(i==1) {
    kospi_mean[i] <- mean(kospi[1:a[i],2])
  } else if(i!=1) {
    kospi_mean[i] <- mean(kospi[(a[i-1]+1):a[i],2])
  }
}
kospi3 <- data.frame(as.Date(kospi2[a,1]), kospi_mean)
colnames(kospi3) <- c("Time", "Mean_Price")
head(kospi3)
tail(kospi3)

plot(kospi3, main= "KOSPI data", type='o')

##### https://www.statmethods.net/index.html #####
