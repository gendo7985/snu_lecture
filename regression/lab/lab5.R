#############
#           #
#   LAB 5   #
#           #
#############

# setting & libraries

set.seed(0)
library(dplyr)
library(MASS)
library(ISLR)
library(car)
library(ggplot2)

########################################

str(Boston)
attach(Boston)

lm_fit <- lm(medv ~ lstat, data = Boston)
summary(lm_fit)

coef(lm_fit)
confint(lm_fit)

predict(lm_fit, data.frame(lstat = c(5, 10, 15)), interval = "confidence")
predict(lm_fit, data.frame(lstat = c(5, 10, 15)), interval = "prediction")

plot(lstat, medv)
abline(lm_fit, lwd = 3, col = "red")

plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)

par(mfrow = c(2, 2))
plot(lm_fit)

par(mfrow = c(1, 1))
plot(predict(lm_fit), residuals(lm_fit))
plot(predict(lm_fit), rstudent(lm_fit))
plot(hatvalues(lm_fit))
which.max(hatvalues(lm_fit))

lm_fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm_fit)

lm_fit <- lm(medv ~ ., data = Boston)
summary(lm_fit)

vif(lm_fit)
lm_fit_1 <- lm(medv ~ . - age, data = Boston)
summary(lm_fit_1)
lm_fit_1 <- update(lm_fit, ~. -age)
summary(lm_fit_1)

summary(lm(medv ~ lstat * age, data = Boston))

lm_fit_2 <- lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(lm_fit_2)

anova(lm_fit, lm_fit_2)

par(mfrow = c(2, 2))
plot(lm_fit_2)
par(mfrow = c(1, 1))

detach(Boston)


str(Carseats)

lm_fit <- lm(Sales ~ . + Income: Advertising + Price: Age, data = Carseats)
summary(lm_fit)

# example 1: x = height, y = weight
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

relation <- lm(y ~ x)
summary(relation)

a <- data.frame(x = 170)
predict(relation, a)

plot(x, y, col = "blue",
     abline(lm(y ~ x)), cex = 1.3, pch = 16,
     main = "Height & Weight Regression",
     xlab = "Height in cm", ylab = "Weight in kg")

predict(relation, newdata = a, interval = "confidence")
predict(relation, newdata = a, interval = "prediction")

pred_int <- predict(relation, interval = "prediction")

mydata <- as.data.frame(cbind(y, x, pred_int))

p <- ggplot(mydata, aes(x, y)) +
  geom_point() +
  stat_smooth(method = lm)
p

p + geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr), color = "red", linetype = "dashed")

# example 2: cars data
data("cars", package = "datasets")
str(cars)

model <- lm(dist ~ speed, data = cars)
summary(model)
plot(model)

new_speed <- data.frame(speed = c(12, 19, 24))

predict(model, newdata = new_speed, interval = "confidence")
predict(model, newdata = new_speed, interval = "prediction")

pred_int <- predict(model, interval = "prediction")

mydata <- cbind(cars, pred_int)

p <- ggplot(mydata, aes(speed, dist)) +
  geom_point() +
  stat_smooth(method = lm)
p

p + geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr), color = "red", linetype = "dashed")


# example 3: iris data
str(iris)

attach(iris)

model_1 <- lm(Petal.Width ~ ., data= iris)
summary(model_1)

model_2 <- lm(Petal.Width ~ Sepal.Length, data = iris)
summary(model_2)

model_3 <- lm(Sepal.Length ~ Petal.Width, data = iris)
summary(model_3)

plot(Petal.Width, Sepal.Length)
abline(model_3, col = "red")

detach(iris)
