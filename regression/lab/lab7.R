#############
#           #
#   LAB 7   #
#           #
#############

# setting & libraries

set.seed(0)
library(segmented)
library(nlme)

#--------------------------------------------

# Polynomial Regression

## Linear relationship between q and y
p <- 0.5
q <- seq(0, 100, 1)
y <- p * q
plot(q, y, type = "l", col = "red", main = 'Linear relationship')

## Non-linear relationship between q and y
y <- 450 + p * (q - 10)^3
plot(q, y, type = "l", col = "navy", main = "Nonlinear relationship")

## How to fit a polynomial regression
q <- seq(0, 20, by = 0.1)
y <- 500 + 0.4 * (q - 10)^3

## add noise
noise <- rnorm(length(q), mean = 10, sd = 80)
noisy_y <- y + noise

## plot of simulated obs.
plot(q, noisy_y, col="deepskyblue4", xlab = "q", main = "Observed data")
lines(q, y, col = "firebrick1", lwd = 3)

## fitting polynomial regression
## K = 3
model_3 <- lm(noisy_y ~ poly(q, 3))
summary(model_3)

## K = 4
model_4 <- lm(noisy_y ~ poly(q, 4))
summary(model_4)

## get confidence interval with confint()
confint(model_3)

## plot of fitted vs residuals
plot(fitted(model_3), residuals(model_3))

## predicted values and confidence intervals
predicted_intv <- predict(model, interval = "prediction", level = 0.99)
head(predicted_intv)

## plotting q vs noisy_y
plot(q, noisy_y, col="deepskyblue4", xlab = "q", main = "Observed data")
lines(q, y, col = "firebrick1", lwd = 3)
lines(q, predicted_intv[, 1], col = "green", lwd = 3)
lines(q, predicted_intv[, 2], col = "black")
lines(q, predicted_intv[, 3], col = "black")
legend("bottomright", c("Obs", "Signal", "Predicted"),
       col=c("deepskyblue4", "red", "green"), lwd = 3)


#--------------------------------------------

# Piecewise Linear Regression

## generate data
data = list(
  x = c(50, 60, 70, 80, 90),
  y = c(703.786, 705.857, 708.153, 711.056, 709.257)
)
plot(data, type = "b")

## psi: starting values for the breakpoints to be estimated
model_lm <- segmented(lm(y ~ x, data = data), psi = 60)
summary(model_lm)

plot(model_lm, col = "red", add = TRUE)


#--------------------------------------------

# Generalized Least Squares

str(airquality)

## linear regression
model_1 <- lm(Ozone ~ Wind, data = airquality)
summary(model_1)

model_2 <- lm(Ozone ~ ., data = airquality)
summary(model_2)

## missing values in object
model_3 <- gls(Ozone ~ Wind, data = airquality)

## 37 NA's
summary(airquality$Ozone)

## na.exclude
model_3 <- gls(Ozone ~ Wind, data = airquality, na.action = na.exclude)
summary(model_3)

## polynomial regression
model_4 <- lm(Ozone ~ poly(Wind, 3), airquality)
summary(model_4)

## backward
model_5 <- lm(Ozone ~ poly(Wind, 2), data = airquality)
summary(model_5)

# psi: piecewise breakpoints
summary(airquality$Wind)
model_6 <- segmented(lm(Ozone ~ Wind, data = airquality), psi = 9)
summary(model_6)

plot(airquality$Wind, airquality$Ozone)
plot(model_6, col='red', add=T)
