# Multivariate analysis lab 
# Written by S. Jung
# Version date: Nov 13, 2019

## Comparisons of several populations and classification


# Example data: flea ------------------------------------------------------
library(dplyr)
library(GGally)  
?flea

# Fairly small dataset on flea beetles
ggpairs(flea)
ggscatmat(flea, columns = 2:4, color = "species")


# Comparisons of several populations  ----------------------------------------
# We compare the means of three difference populations.
# Assuming normality and the equality of covariance, MANOVA provides an answer.

# To test whether the covariances are indeed equal, one may use the likelihood-ratio
# to test the null hypothesis H0: Sigma_1 = Sigma_2 = ... = Sigma_g
# A bias-corrected version of the LRT is called Box's M test
library(MVTests)
BoxM(flea[,2:7], group = flea$species)

# Exercises ---------------------------------------------------------------

# 1. Interpret the result of Box's M test. Can you assume equal covariance?
# 2. Box's M test assumes multivariate normality of each population. 
#    Is such an assumption valid? 
#    Can we assume normality for each population?  (See Lab #2)

# Now let us test whether means of these measurements differ by species.
# For this, explicitly compute the with-in and between covariances (W and B) 

n <- nrow(flea)
p <- ncol(flea) - 1
uniq.id <- unique(flea$species)

mean_vectors <- vector()
W <- matrix(0, nrow = p, ncol = p)
for (gr.id in uniq.id) {
  gData <- as.matrix( flea[flea$species == gr.id,2:7] ) 
  W <- W + t( scale( gData, scale = FALSE))  %*%  (  scale( gData, scale = FALSE) )
  mean_vectors <- cbind(mean_vectors,
                        colMeans(gData))
}
Sp <- W / (n - 3)
Tot <- cov(flea[,2:7]) * (n-1)
B <- Tot - W      


# Exercises ---------------------------------------------------------------

# 3. Manually compute the Between-variance matrix B from "mean_vectors". 
#    Check whether your result is equal to B = Tot - B above. 

# In practice, we simply use the manova() function. The response variable is now 
# the collection of six variables, and a (perhaps non-intuitive) way of 
# indicating response variables is to make them as a matrix
 
out <- manova(as.matrix(flea[,2:7]) ~ species, data = flea)
out
summary(out) 

# Note that by default, Pillai's test statistic is used  
(sout <- summary(out, test = "Pillai"))
summary(out, test = "Wilks")
summary(out, test = "Hotelling-Lawley")
summary(out, test = "Roy")

# Now see if the B and W matrices are computed as expected
# Compare the following with B and W 
sout$SS


# eigenvalues of inv(W)*B
eigen( solve(W) %*% B)


# Explicitly compute Pillai's test statistic 
# tr(B ( B+W)^{-1}) = sum {lambda / (1+lambda)} 
sum(sout$Eigenvalues / (1 + sout$Eigenvalues))

# Pillai's test scales this value, then compares with an F distribution. 

# Exercises ---------------------------------------------------------------

# 4. How many positive eigenvalues should you see in the above? 
# 5. All of the MANOVA Tests above are not valid if normality is not assumed. 
#    For such cases, one may use permutation-based test. 
#    Inspect the following lines and report your conclusion. 

library(purrr)
library(modelr)
perms <- permute(flea,  100, species)
permuted_Pillai <- map(perms$perm,
              ~ summary(manova(as.matrix(flea[,2:7]) ~ species, data = .))$stats[3])
permuted_Pillai <- unlist(permuted_Pillai)
hist(permuted_Pillai)


# Classification ----------------------------------------------------------

# Linear and quadratic discriminant analysis; 
# focus on binary classification, on dim. reduced data

flea.b <- flea %>% filter(species != "Heptapot.")

library(car)
X <- prcomp(flea.b[,2:7])$x %>% as.data.frame %>% 
  dplyr::select(PC1,PC2) %>% mutate(species = factor(flea.b$species))


scatterplot(PC1 ~ PC2 | species , X , smooth=FALSE, regLine=FALSE)

# Use lda() and qda() in package MASS 
library(MASS)
fit <- lda(species ~ ., data = X) 
fit 
abline(0,-fit$scaling[2]/fit$scaling[1],pch=5)

fit2<- qda(species ~ ., data = X) 
fit2 

library(klaR)
partimat(species ~ ., data = X, method="lda",prec=100)
partimat(species ~ ., data = X, method="qda",prec=100) 

# logistic regression (Why the warning message?)
fit3<- glm(species ~ . , data = X, family  = "binomial")
o <- train(species ~ ., data = X, method = "glm")
fit3 

# Compare the in-sample prediction accuracy 
# (Note that we should compare the out-of-sample accuracy)

in.sample.prediction <- predict(fit, X[,1:2])$class
library(caret)
confusionMatrix(X[,3], in.sample.prediction)

in.sample.prediction <- predict(fit2, X[,1:2])$class
confusionMatrix(X[,3], in.sample.prediction)

# Logistic regression returns the logit log(p(1-p))
in.sample.prediction.p <- predict(fit3, X[,1:2])
plot(in.sample.prediction.p)
in.sample.prediction <- factor(ifelse(in.sample.prediction.p < 0, "Concinna", "Heikert." ))
confusionMatrix(X[,3], in.sample.prediction)

# Exercises ---------------------------------------------------------------

# 6. Convert "in.sample.prediction.p" into the posterior probability of "Concinna" given x. 
 

# Exercises on your own ---------------------------------------------------

# 7. By executing the following lines, compare five different methods of classification. 
#    The data are obtained from Kaggle.com (I forgot which entry it was...)
# 8. The codes below only compares k = 3, 5, 7 neighbors in k-NN. Try each value of k in 3:20 
#    and create a line plot, plotting "k vs training accuracy", overlaid with "k vs testing accuracy"

heart <- read.csv("heart_lab06.csv")
summary(heart)
heart$target <- factor(heart$target)

# Split into training and testing data set

set.seed(123)
trainIndex <- createDataPartition(heart$target, p = 0.8, list = FALSE)
train.data <- heart[trainIndex,]
test.data <- heart[-trainIndex,]
 

# Set tuning parameter selection method
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3) 

method.list <- c("lda","qda","glm","knn","nb")
out.list <- list()
for (i in 1:5) {
  out.list[[i]] <- train(target ~ . , 
                         data = train.data, 
                         method = method.list[i], 
                         trControl = control)
} 

# Evaluate the each classifier
confusion.out <- list()
for (i in 1:5){
  confusion.out[[i]] <- confusionMatrix(
    predict(out.list[[i]], newdata = test.data), 
    reference = test.data$target)
}
