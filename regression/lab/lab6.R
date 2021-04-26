#############
#           #
#   LAB 6   #
#           #
#############

# setting & libraries

set.seed(0)

#--------------------------------------------

# Data generation
x1 <- runif(1000, -2, 2)
x2 <- runif(1000, 0, 4)
e <- rnorm(1000)
y <- 1 - 4 * x1 + 3 * x1 ^ 2 + 0.5 * x2 + e

#--------------------------------------------

# Linear regression

## multiple linear regression
fit1 <- lm(y ~ x1 + x2)
summary(fit1)

par(mfrow = c(2, 2))
plot(fit1, which = 1:4)

par(mfrow = c(1, 2))
plot(fit1, which = 5:6)

par(mfrow = c(1, 1))
shapiro.test(residuals(fit1))

fit2 <- lm(y ~ x1 + I(x1^2) + x2)
summary(fit2)

par(mfrow = c(2, 2))
plot(fit2, which = 1:4)

par(mfrow = c(1, 2))
plot(fit2, which = 5:6)

par(mfrow = c(1, 1))
shapiro.test(residuals(fit2))

fit3 <- lm(y ~ x1 + x1^2 + x2) # x1 * x1: interaction term (different from fit2)
summary(fit3)

x1_sq <- x1 ^ 2
fit4 <- lm(y ~ x1 + x1_sq + x2)
summary(fit4)

anova(fit1, fit4) # significant
