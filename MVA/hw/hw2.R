# Exercises ---------------------------------------------------------------
library(mvtnorm)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
set.seed(0)
# 1. Let mu = (0, 1, 2), Sigma_{i,j} = sigma_i * sigma_j * rho^{|i-j|},
# rho = 0.9, (sigma_1, sigma_2, sigma_3) = (3,2,1).
# Randomly sample n = 100 observations following N(mu,Sigma), and 
# compute the sample mean Xbar, covariance matrix S and correlation matrix R.

Sigma_gen <- function(i, j){
  sigma_i <- c(3, 2, 1)
  rho = 0.9
  sigma_i[i] * sigma_i[j] * rho^(abs(i-j))
}

Sigma <- matrix(nrow=3, ncol=3)
for (i in 1:3){
  for (j in 1:3){
    Sigma[i,j] = Sigma_gen(i, j)
  }
}

Rho <- cov2cor(Sigma)
mu <- c(0, 1, 2)

X <- rmvnorm(100, mean=mu, sigma=Sigma)
(Xbar <- colMeans(X))
(S <- cov(X))
(R <- cov2cor(S))

# 2. Compute the difference between Xbar and mu, S and Sigma, 
# R and the population correlation matrix, using the Frobenius norm. 
# Repeat for n = 10, 20, ... 500, and visually display the result. 
# In what rate does the difference reduce? 

norm(as.matrix(Xbar - mu), "F")
norm(S - Sigma, "F")
norm(R - Rho, "F")

Xbars <- c()
Ss <- c()
Rs <- c()
for (n in seq(10, 500, by=10)){
  X <- rmvnorm(n, mean=mu, sigma=Sigma)
  Xbars[n/10] <- norm(as.matrix(colMeans(X) - mu), "F")
  Ss[n/10] <- norm(cov(X) - Sigma, "F")
  Rs[n/10] <- norm(cor(X) - Rho, "F")
}
iter <- seq(10, 500, by=10)
data <- data.frame(c(Xbars, Ss, Rs))
data <- data %>% mutate(n=rep(iter,3), type=rep(c("Xbar", "S", "R"), each=50))
colnames(data)[1] <- "F_norm"

ggplot(data, aes(x=n, y=F_norm, color=type))+
  geom_point()

lm(Xbars ~ iter) # rate: -0.0007
lm(Ss ~ iter)    # rate: -0.004
lm(Rs ~ iter)    # rate: -0.00008

# 3. Suggest how would you generate a non-normal 3-variate distribution. 
# The three variables should be (linearly) correlated with each other. Randomly
# sample n = 100 observations from the non-normal distribution. Empirically 
# confirm that the three variables are indeed correlated with each other 
# by 1) computing the sample correlation coefficients and 2) displaying the scatter. 
# (The scatterplot should also exhibit the non-normality to some degree.)

X1 <- runif(100)
X2 <- 2 * X1 + 2 * runif(100, -5, 0)
X3 <- -X1 + X2 - 4 * runif(100, -5, 5)
X.new <- data.frame(cbind(X1, X2, X3))
cor(X.new)
ggpairs(X.new)

# 4. To check normality (rather, non-normality) of the data you have generated, 
#  a) use the normal-probability plot for each variable to check normality; 
#  b) Use the Shapiro-Wilk test (cf `?shapiro.test`). Be sure to specify 
# the null hypothesis of the test;
#  c) Use the chi-squared plot to check normality. 

# a) QQ-plot
qqnorm(X1); qqline(X1)
qqnorm(X2); qqline(X2)
qqnorm(X3); qqline(X3)

# b) Shapiro-Wilk test
shapiro.test(X1) # p-value = 0.002, reject
shapiro.test(X2) # p-value = 0.02 , reject
shapiro.test(X3) # p-value = 0.001, reject

# c) Chi-squared plot
S <- cov(X.new)
Mdist <- mahalanobis(X.new, colMeans(X.new), cov(X.new))
qqplot(qchisq(ppoints(100), df = 3), Mdist)

# 5. Suppose (X,Y,Z)  follow the 3-variate normal distribution defined in #1. 
#  a) What is the conditional distribution of (X,Y) given Z = z? 
#  b) What is the best linear prediction of (X,Y) as a function of Z, i.e. BLP(X,Y | Z)?
#  c) What is multiple correlation coefficient between Z and (X,Y)? 
#  d) Randomly sample n = 1000 observations as in #1. Using the data, perform 
#     a linear regression analysis of regressing Z onto (X,Y). Compare the R^2 
#     from the regression with your answer in the subproblem c). 

# a) Conditional dist. of (X, Y) given Z = z
#    mu  = (0, 1) + (2.43, 1.8) / 1 * (z - 2)
#        = (2.43(z - 2), 1.8(z - 2))
#    cov = [9, 5.4] - [2.43] * [2.43, 1.8]
#          [5.4, 9]   [1.8 ]
#        = [3.0951,  1.026]
#          [1.026 ,  0.760]
#    (X, Y) ~ N(mu, cov)

# b) BLP(X, Y | Z)
A <- Sigma[1:2, 3, drop=F] # Sigma[3,3] = 1
b <- mu[1:2] - A %*% mu[3]
BLP <- as.matrix(X[,3]) %*% t(A) + as.vector(b)

# c) m.corr(Z, (X,Y)) = corr(Z, BLP(Z|X, Y))
Sigma[3, 1:2] %*% solve(Sigma[1:2, 1:2]) %*% Sigma[1:2, 3, drop=F] / Sigma[3, 3]

# d)
X <- as.data.frame(rmvnorm(1000, mean=mu, sigma=Sigma))
colnames(X) <- c("X", "Y", "Z")
lm.out <- lm(Z ~ ., data = X)
summary(lm.out)$r.squared # 0.792
