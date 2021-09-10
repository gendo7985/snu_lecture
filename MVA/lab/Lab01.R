# Multivariate analysis lab 
# Written by S. Jung 

## Lab 1: Multivariate data exploration

library(ggplot2) 

# Checking data -----------------------------------------------------------

data(mpg)
?mpg
View(mpg)
dim(mpg)
nrow(mpg)
ncol(mpg)
str(mpg)
attributes(mpg)
rownames(mpg)
colnames(mpg)
variable_names <- variable.names(mpg)

# Select variables --------------------------------------------------------

# By variable names
mpg[,"hwy"]
mpg[,c("displ","cty")]

# By indices
mpg[,2]
mpg[,-2]
mpg[,c(1,2)]

# By logical vectors
Binary_indicator <- variable_names %in% c("cty","drv")
mpg[,Binary_indicator]

Numeric_indicator <- unlist(lapply(mpg, is.numeric))
mpg[,Numeric_indicator]

# Using dplyr::select
library(dplyr)
mpg %>% select(displ, hwy)
mpg %>% select(displ:hwy)
mpg %>% select(-displ,-hwy) 
 

# Subsetting observations -------------------------------------------------

# By row names
data(USArrests)
rownames(USArrests)
USArrests["Alabama",]

# By indices
n <- nrow(mpg)
mpg[1:5,]
mpg[1:floor(n/2),]
mpg[sample(1:n, size = 10, replace = TRUE),]

test_sample_id <- sample(1:n, size = floor(n/5), replace = FALSE)
train_sample_id <- setdiff(1:n, test_sample_id) 
mpg[train_sample_id,]
mpg[-test_sample_id,]

# By logical vectors
is_audi <- mpg$manufacturer == "audi" 
mpg[is_audi,]

# Using dplyr::filter
mpg %>% filter(manufacturer == "audi")
mpg %>% filter(displ > 3)
mpg %>% filter(displ > 3 & year <= 2000)
mpg %>% sample_frac(0.5)
mpg %>% sample_n(3,replace = TRUE)


# Descriptive Statistics --------------------------------------------------
X <- USArrests

apply(X, 2, mean)
colMeans(X)
var(X)
diag(var(X))
cor(X)

n <- nrow(X)
p <- ncol(X)


# Visualization: scatterplots ---------------------------------------------
plot(USArrests[,1:2])

USArrests %>% ggplot(aes(x = Murder, y = Assault)) + geom_point()
USArrests %>% ggplot(aes(x = Murder, y = Assault, size = UrbanPop)) + geom_point()

mpg %>% ggplot(aes(x = cty, y = hwy)) + geom_point()
mpg %>% ggplot(aes(x = cty, y = hwy, color = as.factor(cyl))) + geom_point()

pairs(USArrests)
pairs(USArrests,col = state.region)
 
library(GGally) # See https://ggobi.github.io/ggally/
ggpairs(USArrests)

ggscatmat(USArrests)
USArrests.m <- cbind(USArrests, state.region)
ggscatmat(USArrests.m, columns = 1:4,color = "state.region")



# Covariance matrix and its inverse ---------------------------------------

S <- cov(X)
solve(S) # S^{-1}.
S %*% solve(S) == diag(1,p)   
norm(S %*% solve(S) - diag(1,p), "F" )
norm(S %*% solve(S) - diag(1,p), "2" )
all.equal( S %*% solve(S)   , diag(1,p), check.attributes = FALSE )


ggcorr(X)
library(corrplot)
corrplot(cor(X)) 


# Distances ---------------------------------------------------------------

# Euclidean distance
sqrt( sum( (X[1,] -  X[2,])^2 ) )
c <- X[1,] -  X[2,]
sqrt(sum(c*c))

is.matrix(c)
c <- as.matrix(c)

sqrt( c %*% t(c) )  ## try t(c) %*% c
norm( c, type = "F") 
norm( c, type = "2") # these two are the same as c is essentially a vector

# Mahalanobis distance 
Xbar <- colMeans(X)
y <- X[1,]
mahalanobis(y, Xbar, cov(X)) # returns squared distance 
as.matrix(Xbar - y) %*% solve(cov(X)) %*% t(Xbar-y)
sqrt(as.matrix(Xbar - y) %*% solve(cov(X)) %*% t(Xbar-y))


# Multivariate order from the center 
n <- nrow(X)
xdist <- rep(0,n)
for(i in (1:n)){
  xdist[i]=as.matrix(X[i,]- Xbar) %*% solve(cov(X)) %*% t(X[i,]- Xbar)
}
hist(xdist)
rank(xdist)


# Projections and change of variables -------------------------------------

# V: an orthogonal system
V <- eigen(cov(X))$vectors 
V %*% t(V) 
all.equal(V %*% t(V), diag(1,4))
# project onto the first two column vectors 
rotX <- as.matrix(X) %*% V[,1:2]
rotX <- as.data.frame(rotX)
colnames(rotX) <- c("NewV1", "NewV2")
ggpairs(rotX)


# Exercises ---------------------------------------------------------------

# 1. Using the datasets `USArrests` and `state`, 
# create a data frame containing all 50 states of USA 
# measured for area (size), region, and four types of crime rates. 
# Resulting dataset must have 5 numeric variables and one categorical variable.

# 2. Draw a scatterplot matrix using all numeric variables, 
# colored by different regions.

# 3. Compute the sample mean for each region. 

# 4. Overlay the five sample means on the scatterplot matrix. 
# Add appropriate labels.

# 5. Ignore the categorical variable. For each observation, 
# compute the mahalanobis distance from the (grand) sample mean
# and use it to sort from the closest to furthest from the mean. 
# Report the ordering. 

# 6. Consider the plane containing the mean (computed above) and 
# the two furthest states from the mean. Create orthonormal basis 
# spanning the plane, and display the projections of all observations 
# on the plane. Be sure to label the points with their names (or abbr's.)

# 7. Now use the eigenvectors (cf: eigen(cov(X))$vectors) to change 
# the coordinates. Compute the sample covariance matrix in the new coordinate system.


