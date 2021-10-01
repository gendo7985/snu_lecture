# Multivariate analysis lab 
# Written by S. Jung 

## Lab 2: Association between random vectors and multivariate normal distribution

# Regression and best linear prediction --------------------------

library(ISLR)
library(dplyr)
library()
data("Auto")
Auto <- as_tibble(Auto)
Auto

# Consider regressing mpg on displacement, horsepower and weight 
X <- Auto %>% select(1,3:5)
colnames(X) <- c("y","x1","x2","x3")

lm.out <- lm(y ~ ., data = X)
summary(lm.out) 

# Using the estimates of means and covariances, find 
# 1) the best linear prediction of X2 "Y" as a linear function of X1 "x1,x2,x3"

i1 <- c(F,T,T,T) # block 1 indices 
colnames(X)[i1]
i2 <- c(T,F,F,F) # block 2 indices

mu <- colMeans(X)
Sigma <- as.matrix(var(X))
Sigma <- var(X)

mu1 <- mu[i1]
mu2 <- mu[i2]
Sigma11 <- Sigma[i1,i1]
Sigma12 <- Sigma[i1,i2]
Sigma21 <- Sigma[i2,i1]
Sigma22 <- Sigma[i2,i2]

# BLP(X2 | X1) = AX_1 + b, where 
# A =  Sigma_{21}Sigma_{11}^{-1}
# b =  mu2 - Sigma_{21} Sigma_{11}^{-1} mu1

A <- Sigma21 %*% solve(Sigma11)
b <- mu2 - A %*% mu1
c(b,A)

# Compare these with the fitted regression coefficients
lm.out$coefficients

# Compute the BLP and compare with the fitted values
BLP <- as.matrix(X[,i1]) %*% t(A) + as.vector(b)
all.equal(as.vector(BLP), lm.out$fitted.values, check.attributes = FALSE)

# Compute the "residual" (X2 - BLP(X2|X1)) and compare with the regression residuals
resid <- as.matrix(X[,i2]) - BLP
all.equal(as.vector(resid), lm.out$residuals, check.attributes = FALSE)




# Regression and multiple correlation coefficient -------------------------

# m.corr(X2, X1) = corr(X2, BLP(X2|X1)) 
#                = sqrt{Sigma21 * Sigma11^{-1} * Sigma12 / Sigma22 }

# The squared multiple corr. coef. 
(m.corr2 <- Sigma21 %*% solve(Sigma11) %*% Sigma12 / Sigma22 )

# compar with the coff. of determination (R-squared)
summary(lm.out)$r.squared


# Partial covariance and correlation coef. -------------------------
i1 <- c(T,T,F,F) # block 1 indices, for "mpg" and "disp" 
colnames(X)[i1]
i2 <- !i1        # block 2 indices, for the rest of variables

mu1 <- mu[i1]
mu2 <- mu[i2]
Sigma11 <- Sigma[i1,i1]
Sigma12 <- Sigma[i1,i2]
Sigma21 <- Sigma[i2,i1]
Sigma22 <- Sigma[i2,i2]

# Partial covariance matrix of "y" and "x1"   and 
# Partial correlation coef. of "y" and "x1", after removing 
# the effect of the rest
(p.cov <- Sigma11 - Sigma12 %*%solve(Sigma22) %*% Sigma21)
(p.corr <- cov2cor(p.cov))


# The coefficient for x1 in the multiple regression y ~ x1+x2+x3 is
# exactly the simple regression coefficient "beta" of regressing 
# y* = y-BLP(y|x2,x3) onto x* = x1 - BLP(x1|x2,x3).

# 1) When the covariance of y* and x* is known, then beta = cov(y*,x*) / cov(x*,x*)
p.Sxy <- p.cov[1,2]
p.Sxx <- p.cov[2,2]
(Opt.Beta <- p.cov[1,2]/ p.cov[2,2])

# 2) Or
resid.y <-  lm(y ~ .-x1, data = X)$residuals
resid.x <-  lm(x1 ~ .-y, data = X)$residuals
(Opt.Beta2 <- cov(resid.y,resid.x) / var(resid.x))


# 3) Compare with
lm.out$coefficients
lm(resid.y~resid.x)$coefficients 





# Multivariate normal Random number generation --------------------------------

## By package "mvtnorm"
library(mvtnorm)
mu <- c(5,10)
Sigma <- matrix(c(4,3,
                  3,6), nrow = 2)
n <- 100
X <- rmvnorm(n, mean= mu, sigma = Sigma)
# resulting in a n-by-p matrix 

## Or using transformation;

# X ~ AZ + mu, Z ~ N(0,I), if AA' = Sigma. 
A <- t(chol(Sigma)) # Using Cholesky decomposition ()
# check A%*%t(A) = Sigma

A <- with(eigen(Sigma),
          vectors %*%  diag( sqrt( values ) ) %*% t(vectors) 
) # Using eigen-decomposition (symmetric sqrt) 
# check A = t(A) and A%*%t(A) = A%*%A = Sigma

p <- 2
n <- 100
X.transposed <- A %*% matrix(rnorm(n*p), nrow = p) + mu 
X <- t(X.transposed)



# Checking normality ------------------------------------------------------
# See textbook Sections 4.6 - 4.8.

# By normal-probability plot (for each marginal dist'n)
qqnorm(X[,1]); qqline(X[,1])

# By comparing the Mahalanobis distances with x^2-dist'n
Xc <- t(t(X) - colMeans(X))
S <- cov(X)
Mdist <- sqrt( diag( Xc %*% solve(S) %*% t(Xc) ) ) 
qqplot( qchisq(ppoints(n), df = p), Mdist^2)




# Exercises ---------------------------------------------------------------

# 1. Let mu = (0, 1, 2), Sigma_{i,j} = sigma_i * sigma_j * rho^{|i-j|},
# rho = 0.9, (sigma_1, sigma_2, sigma_3) = (3,2,1).
# Randomly sample n = 100 observations following N(mu,Sigma), and 
# compute the sample mean Xbar, covariance matrix S and correlation matrix R.

# 2. Compute the difference between Xbar and mu, S and Sigma, 
# R and the population correlation matrix, using the Frobenius norm. 
# Repeat for n = 10, 20, ... 500, and visually display the result. 
# In what rate does the difference reduce? 

# 3. Suggest how would you generate a non-normal 3-variate distribution. 
# The three variables should be (linearly) correlated with each other. Randomly
# sample n = 100 observations from the non-normal distribution. Empirically 
# confirm that the three variables are indeed correlated with each other 
# by 1) computing the sample correlation coefficients and 2) displaying the scatter. 
# (The scatterplot should also exhibit the non-normality to some degree.)

# 4. To check normality (rather, non-normality) of the data you have generated, 
#  a) use the normal-probability plot for each variable to check normality; 
#  b) Use the Shapiro-Wilk test (cf `?shapiro.test`). Be sure to specify 
# the null hypothesis of the test;
#  c) Use the chi-squared plot to check normality. 

# 5. Suppose (X,Y,Z)  follow the 3-variate normal distribution defined in #1. 
#  a) What is the conditional distribution of (X,Y) given Z = z? 
#  b) What is the best linear prediction of (X,Y) as a function of Z, i.e. BLP(X,Y | Z)?
#  c) What is multiple correlation coefficient between Z and (X,Y)? 
#  d) Randomly sample n = 1000 observations as in #1. Using the data, perform 
#     a linear regression analysis of regressing Z onto (X,Y). Compare the R^2 
#     from the regression with your answer in the subproblem c). 