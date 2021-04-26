#############
#           #
#   LAB 4   #
#           #
#############

# setting & libraries

set.seed(0)
library(dplyr)

#--------------------------------------------

# Multiple linear regression

## generating random variables
x1 <- runif(50, min = 5, max = 25)
x2 <- runif(50, min = 0, max = 5)
e <- rnorm(50, sd = 5)
y <- 1 + 2 * x1 + 3 * x2 + e

D <- data.frame(Y = y, x1 = x1, x2 = x2)
X <- model.matrix(Y ~ ., data = D)

## multiple regression
fit_lm <- lm(Y ~ ., data = D)
summary(fit_lm)

## structure of lm
str(fit_lm)

## coefs
fit_lm[[1]]
fit_lm[['coefficients']]
coef(fit_lm)

## coefs name
attr(fit_lm[['coefficients']], "names")
fit_lm %>% coef %>% names

## residuals
fit_lm[['residuals']]
residuals(fit_lm)

## SSR
deviance(fit_lm)
residuals(fit_lm)^2 %>% sum

## confidence Interval
confint(fit_lm)

## predict value
predict(fit_lm, data.frame(x1 = 0, x2 = 0))

## structure of summary(lm)
fit_lm_summary <- summary(fit_lm)
str(fit_lm_summary)

## adjusted R^2
fit_lm_summary[['adj.r.squared']]


#--------------------------------------------

# Beta with matrix calculation
solve(t(X) %*% X) %*% t(X) %*% as.matrix(D['Y'])
coef(fit_lm)

# Residual Sum of Squares
deviance(fit_lm)
sum(residuals(fit_lm)^2)

# sigma = sqrt(MSE)
fit_lm_summary[['sigma']]
sum(residuals(fit_lm)^2 / (49 - 2)) %>% sqrt

# Covariance Matrix
cov_mat <- solve(t(X) %*% X) * deviance(fit_lm) / 47
cov_mat
diag(cov_mat) %>% sqrt
summary(fit_lm)


#--------------------------------------------

# Experiment_1: with intercept
coef(fit_lm)[3]

e_y_x1 = residuals(lm(Y ~ x1, data = D))
e_x2_x1 = residuals(lm(x2 ~ x1, data = D))

coef(lm(e_y_x1 ~ e_x2_x1))[2]

# Experiment_2: without intercept
coef(lm(Y ~ . -1, data = D))[2]

e_y_x1 = residuals(lm(Y ~ x1 - 1, data = D))
e_x2_x1 = residuals(lm(x2 ~ x1 - 1, data = D))

coef(lm(e_y_x1 ~ e_x2_x1 - 1))[1]
