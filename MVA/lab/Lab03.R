# Multivariate analysis lab 
# Written by S. Jung 

## Lab 3: Wishart distribution and inference on multivariate location 

# In this lab, we will explore 

# (1) Distribution of the Hoteling's T^2 and its variants. 
# (2-3) Wishart distribution, and an inference problem related to the Wishart parameter
# (4) Exercises for inference on multivariate locations 


# (I) Distribution of the Hoteling's T^2 and its variants. --------------

library(mvtnorm)
p <- 2
mu <- c(5,10)
Sigma <- matrix(c(4,3,
                  3,6), nrow = 2)
n <- 100
X <- rmvnorm(n, mean= mu, sigma = Sigma) # A n x p normal data matrix 
Xbar <- colMeans(X)
S <- cov(X) 

# Compute Hoteling's T statistics (scaled appropriately to follow an F distribution)
T2 <- (n-p) / p * n / (n-1) *t(as.matrix(Xbar - mu)) %*% solve(S) %*% as.matrix(Xbar - mu)
Fprob <- df(T2,p,n-p) # and the tail probability of F-distribution

# Comparison with a pre-packaged computation
library(ICSNP)
a <- HotellingsT2(X, mu = mu, test = "f") 
a$statistic
a$p.value

 
# (II) Wishart distribution -----------------------------------------------

# A Wishart-distributed random matrices can be sampled using
# i) Random normal variates (through the definition)
# ii) by rWishart function 

p <- 3
Sigma <- matrix(c(10,3,1,
                  3,6,1,
                  1,1,6), nrow = 3)
m <- 100
X <- rmvnorm(n = m , sigma = Sigma)
M <- t(X) %*% X # just one random matrix 


# Now sample n = 100 many Wishart matrices
M.array <- rWishart(n = 500, df = m, Sigma = Sigma)

# Inspect the first two observations 
M.array[,,1]
M.array[,,2]

# Take a sample mean and (element-wise) variance and compare with their expected value. 
apply(M.array,c(1,2),mean)
apply(M.array,c(1,2),var)
 

# Exercise 
# 1. Write down the population mean and population "element-wise" variance of the random
# Wishart matrix. Use the Frobenius Norm on the different between the empirical and population 
# mean matrices (and variance matrices)  to record the deviation. (Google "Frobenius norm" for 
# definition.) What will happen to your deviation if $m$ increases (or $n$ increases)? 



# (III) An inference problem for the Wishart parameter --------------------

# Let $M \sim Wishart(m, Sigma)$. Consider testing the null hypothesis of 
# $\Sigma = I$ vs $\Sigma \neq I$ based on only one observation of M. 
# We will construct a likelihood ratio test. 
# Assume $m > p$ and recall the density function of the Wishart matrix. 
# Then, i) $M/m$ is the m.l.e. of Sigma.
#       ii) The LR statistic, $-2 \log (L_0 / L_1)$ is a function of eigenvalues $\lambda_i$ of $M/m$, 
#          and is $ m \sum_{i=1}^k (\lambda_i - 1 - \log(\lambda_i))$.
#       iii) Under H_0, the LR statistic follows chi^2 distribution with d.f. p(p+1)/2 (if m is large)

p <- 3
m <- 100
M  <- rWishart(n = 1, df = m, Sigma = diag(rep(1,p)))
lambdas <- eigen(M[,,1] / m )$values
m * ( sum(lambdas)  - p - sum(log(lambdas)))


# Exercise 
# 2. Verify the distribution of the LR statistic is indeed chi^2, using a qqplot.  

        
# (IV) Exercises for inference on multivariate locations ----------------------------
# Textbook problem 5.9 (Demonstration)

# Exercise
# 3. Continued from Problem 5.9. Test whether means of the variables are (100, 150, 50, 90, 17, 31). 
# Provide a p-value. 




