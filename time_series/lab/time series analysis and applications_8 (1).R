# rm(list=ls())

# install.packages("ggplot2")
library(forecast)
library(quantmod)
library(tseries)
library(ggplot2)

?decompose

## data
z <- scan("food.txt")
head(z, 24)

data_sample=ts(z, start=c(1981,1), frequency=12) # ?ts
data_sample

plot(data_sample)

## unit root test
adf.test(data_sample)
adf.test(data_sample, k=12) ## lag order

## how to find period (trend component)
findfrequency(data_sample) # ?findfrequency

acf(data_sample)
acf(diff(data_sample))

lag.plot(data_sample, set = c(1:16), pch = ".", col = "gold") # ?lag.plot

# trend component
plot(data_sample)
data_trend = ma(data_sample, 10) # ?SMA
lines(ts(data_trend, start=c(1981,1), frequency=12) , col=4)

ma(1:30, 4, centre = F)
SMA(1:30, 4)

# trend adjustment
data_TA=data_sample-data_trend
plot(data_TA, type = 'l')
acf(na.omit(data_TA))

findfrequency(data_TA)
lag.plot(na.omit(data_TA), set = c(1:16), pch = ".", col = "gold")

# seasonal component
length(data_sample)
length(na.omit(data_TA))
data_seasonal = c()
month = c(rep(c(6:12, 1:5), 11), 6, 7)
for(i in 1:12){
  data_seasonal[i] = mean(na.omit(data_TA)[month==i]) # mean
}
data_seasonal = ts(rep(data_seasonal, 12), start=c(1981,1), frequency=12)
plot(data_seasonal, type = 'l')

data_SA = data_sample-data_seasonal
plot(data_SA, type = 'l')

# error component(remainder)
data_error = data_sample-data_trend-data_seasonal
plot(data_error, type = 'l')

# decomposed plot
par(mfrow=c(2,2))
plot(data_sample, type = 'l')
plot(data_trend, type = 'l')
plot(data_seasonal, type = 'l')
plot(data_error, type = 'l')
par(mfrow=c(1,1))

## decompose function
data_decompose <- decompose(data_sample, "additive")
plot(data_decompose)
data_decompose$seasonal
data_decompose$trend
data_decompose$random

# stl; Seasonal Decomposition of Time Series by Loess(local regression)
data_stl = stl(data_sample, "periodic")
plot(data_stl)


########## simple plot 1 ##########
weather <- read.csv("weather.csv") ;head(weather)
weather_28 <- weather[weather[,2]<29,]
weather_28[1:252,1] <- paste(0, weather_28[1:252,1], sep="")
weather_28[,1] <- paste(weather_28[,1], "월", sep="")
weather_28[,1] <- as.factor(weather_28[,1])
head(weather_28); tail(weather_28)

## time series plot
ggplot(data = weather_28, aes(x = 일, y = 평균.기온)) +
  geom_line()

## time series plot with group
ggplot(data = weather_28, aes(x = 일, y = 평균.기온, group=월)) +
  geom_line()

## time series plot + colour
ggplot(data = weather_28, aes(x = 일, y = 평균.기온, group=월, colour=월)) +
  geom_line()

## time series plot : By month !!!!
ggplot(data = weather_28, aes(x = 일, y = 평균.기온, group=월, colour=월)) +
  geom_line() +
  facet_wrap(~ 월)


########## simple plot 2 ##########
weather2 <- read.csv("weather2.csv")
weather2_28 <- weather2[weather2[,2]<29,]
weather2_28[1:252,1] <- paste(0, weather2_28[1:252,1], sep="")
weather2_28[,1] <- paste(weather2_28[,1], "월", sep="")
weather2_28[,1] <- as.factor(weather2_28[,1])
head(weather2_28); tail(weather2_28)

## +지역
weather_28[,4] <- "서울"; tail(weather_28)
weather2_28[,4] <- "부산"; tail(weather2_28)
weather_bind <- rbind(weather_28, weather2_28)
colnames(weather_bind)[4] <- "지역"; head(weather_bind); tail(weather_bind)
rownames(weather_bind) <- 1:672 ; tail(weather_bind)


## multiple time series plot : By month
ggplot(data = weather_bind, aes(x = 일, y = 평균.기온, group=지역, colour=월)) +
  geom_line() +
  facet_wrap(~ 월)

ggplot(data = weather_bind, aes(x = 일, y = 평균.기온, group=지역, colour=월)) +
  geom_line() +
  facet_wrap(~ 월) +
  theme_bw()

ggplot(data = weather_bind, aes(x = 일, y = 평균.기온, group=지역, colour=월)) +
  geom_line() +
  facet_wrap(~ 월) +
  theme_bw()  +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

## colour=지역
ggplot(data = weather_bind, aes(x = 일, y = 평균.기온, group=지역, colour=지역)) +
  geom_line() +
  facet_wrap(~ 월) +
  theme_bw()

## multiple time series plot + smoothing lines
ggplot(data = weather_bind, aes(x = 일, y = 평균.기온, group=지역, colour=지역)) +
  geom_line() +
  facet_wrap(~ 월) +
  theme_bw() +
  geom_smooth()

## multiple time series plot + smoothing lines with linear model
ggplot(data = weather_bind, aes(x = 일, y = 평균.기온, group=지역, colour=지역)) +
  geom_line() +
  facet_wrap(~ 월) +
  theme_bw() +
  geom_smooth(method=lm)
