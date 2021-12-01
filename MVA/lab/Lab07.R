# Multivariate analysis lab 
# Written by S. Jung
# Version date: Nov. 18, 2019

## Clustering

library(MASS)
library(car)

# We use iris data set for illustration 
# Note the label in the iris data set calls for comparisons of group, not clustering.
# Clustering is typically exploited to search for (unknown and potential) clusters in data
data(iris) 

# k-means with the given value of cluster (k = 3) -------------------------
 
k <- 3
kmeansobj<-kmeans(iris[1:4],k)
kmeansobj
# The resulting cluster
pairs(iris[,1:4],col = c("red", "green3", "blue")[kmeansobj$cluster] )
# To be compared with true labels of the data 
pairs(iris[,1:4],col = c("red", "green3", "blue")[unclass(iris$Species)] )

# It is always good idea to apply random start
(kmeansobj <- kmeans(iris[,1:4], 3, nstart = 25))

# extract cluster membership
kmeansobj$cluster

# Number of clusters; Gap statistic -------------------------
 
library(cluster)
gap <- clusGap(iris[1:4], FUN = kmeans, K.max = 8)
plot(gap)
# Gap statistics suggests hatK = 3 as 
# Gap(1) < Gap(2) - s(2), and
# Gap(2) < Gap(3) - s(3), and
# Gap(3) > Gap(4) - s(4). 


# Exercises ---------------------------------------------------------------

# 1. Load the dataset "rattle.data::wine". Standardize the data. 
# 2. Use the last 12 variables (just excluding "Type") to
#   a) determine the number of clusters;
#   b) visualize your clustering results (on a 2-D coordinate);
#   c) create a confusion table, comparing the clustering results with true labels.


# Hierarchical clustering -------------------------------------------------
  
d = dist(iris[1:4])

## Hierarchical
hc.complete <- hclust(d, method="complete")
hc.average <- hclust(d, method="average")
hc.single <- hclust(d, method="single")

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)

# extract cluster membership 
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
(membership <- cutree(hc.single, 4))

membership <- cutree(tree.avg, k = 3)
pairs(iris[,1:4],col = c("red", "green3", "blue")[membership] )
 

# Gaussian mixture estimation by EM  --------------------------------------

# Assume a two-Gaussian mixture for rattle.data::wine.
# Assuming normality, we will estimate mean vectors, covariance matrices and proportions. 
# 
# The EM algorithm runs as follows. 
library(mvtnorm)
library(dplyr)
par(mfrow=c(1,1))


# 1. Initial value selection. Let us use k-means with K = 2. 

X <- rattle.data::wine %>% select(-Type) %>% scale()
n <- nrow(X)
kmeansobj<-kmeans(X,2)

mu1 <- colMeans( X[kmeansobj$cluster == 1,] )
mu2 <- colMeans( X[kmeansobj$cluster == 2,] )
S1 <- cov( X[kmeansobj$cluster == 1,] )
S2 <- cov( X[kmeansobj$cluster == 2,] )
pi1 <- kmeansobj$size[1] / n 
pi2 <- 1- pi1 

# 2. E-step 
# In the E-step, compute P(Label = 1 | X) by Bayes rule 

pi1_X <- pi1 * dmvnorm(X, mean = mu1, sigma = S1) # P(X, Label = 1)
pi2_X <- pi2 * dmvnorm(X, mean = mu2, sigma = S2) # P(X, Label = 2)
pi1X <-  pi1_X/ (pi1_X + pi2_X)                   # P(Label = 1 | X)
plot(pi1X)
sum(pi1X)

# 3. M-step 
# In the M-step, 
# update mu1 (and S1) by the weighted mean (cov) of X (with weight pi1X);
# update mu2 (and S2) by the weighted mean (cov) of X (with weight 1 - pi1X);
# update pi1 (and pi2) by average of pi1X (and of 1-pi2x).

mu1 <- apply(X, 2, weighted.mean, w = pi1X) 
cX1 <- apply(X, 1, function(x) x - mu1) # transposed (weighted)mean-cented matrix
S1 <- cX1 %*% diag(pi1X) %*% t(cX1) / sum(pi1X)

mu2 <- apply(X, 2, weighted.mean, w = 1 - pi1X) 
cX2 <- apply(X, 1, function(x) x - mu2) # transposed (weighted)mean-cented matrix
S2 <- cX2 %*% diag(1-pi1X) %*% t(cX2) / sum(1 - pi1X)

pi1 <- sum(pi1X) / n 
pi2 <- 1 - pi1

mu1

# Exercises ---------------------------------------------------------------

# 3. Repeat the E and M steps until the estimates do not change substantially. 
#    How many iterations do you need for the change in pi1 is less than 10e-5? 
# 4. Report the clustering result by providing the class probabilities for each observation. 
 

## EM algorithm for Gaussian mixture with structured covariance matrices 
# Modal choices are in the covariance models and in the number of components 

library(mclust)
mixclust = Mclust(iris[1:4])
summary(mixclust)
plot(mixclust)
?mclustModelNames

mixclust = Mclust(iris[1:4], G = 3) # Set 3 clusters
summary(mixclust)
plot(mixclust)
