library(psych)
library(psychTools)

data(epi.bfi)
fit <- factanal(epi.bfi, 6, rotation="none")
print(fit, digits=2, cutoff=.3, sort=FALSE)


# Exercises ---------------------------------------------------------------

# 1. With p = 13, k = 6. Verify the degrees of freedom of the chi-square statistic is correct. 
p <- 13; k <- 6 ; df <- p*(p+1)/2 - p*k -p + k*(k-1)/2
df # 15

# 2. What is the correlation between epiE and Factor1?
# Corr(Xj, Fi) = Cov(Xj, Fi) = lambda_ji
fit$loadings[1,1]
help(epi.bfi)
# 3. Verify that Factor1 is about Extraversion and Factor2 is about Depression and Anxiety. 
#    Continue interpreting Factors 3 to 6. 
# Factor1 is about epiE, epiS = Extraversion
# Factor 2 is about bdi, traitanx, stateanx = Depression, Anxiety
# Factor 3 is about bfagree = Agreableness
# Factor 4 is about epiImp = Extraversion
# Factor 5 is about epiNeur, bfneur = Neuroticism
# Factor 6 is about bfext, bfneur, bfopen = Big 5
help(epi.bfi)

# 4. What is fit$correlation?
fit$correlation
# correlation matrix of epi.bfi

# Exercises ---------------------------------------------------------------
# Take the Olympic Decathlon data at ade4, which contains the performances 
# of 33 men's decathlon at the Olympic Games (1988).
library(ade4)
data(olympic)
help(olympic)

# We will use the portion of data at "tab"
# Create a scree plot using psych::scree
scree(olympic$tab)

# 5. How many factors will you try first? 
k <- 2 # 2 factors

# 6. Use the principal component estimate to fit a factor model. Use varimax rotation. 
fa.out <- fa(olympic$tab, nfactors = k, fm = "pa", rotate = "varimax", scores = "regression")
print(fa.out,digits=2, cut=.3)
# 7. Plot the scatters of factor scores and inspect whether there is an outlier. 
par(mfrow=c(1,1))
plot(fa.out$scores)
text(1.5,-2,"outlier", col="red")
text(2,0.3,"outlier", col="red")
text(0.6,2,"outlier", col="red")
text(1.5,2,"outlier", col="red")
# 8. Use the maximum likelihood estimate to fit a factor model, using varimax rotation. 
#    Are the results from PC estimates similar to those from ML estimates? 
#    Use a graphical method to present the structure and answer: Are the factors interpretable? 
fa.out2 <- fa(olympic$tab, nfactors = k, fm = "ml", rotate = "varimax", scores = "regression")
fa.out$loadings
fa.out2$loadings
par(mfrow=c(1,2))
plot(fa.out$scores, main="PA")
plot(fa.out2$scores, main="ML")
# similar plots, similar loadings
# PA1 is about 100, long, 400, 110 => running ability
# PA2 is about poid, disq => throwing ability

# 9. Try other numbers of factors. 
fa.out3 <- fa(olympic$tab, nfactors = 3, fm = "pa", rotate = "varimax", scores = "regression")
print(fa.out3$loadings,digits=2, cut=.3)
# 2 factor are more easy to interpret!

# 10. Inspect the relation between factor scores and olympic$score. 
fit.out <- lm(olympic$score ~ fa.out$scores)
summary(fit.out)
# score = -353.17 * PA1 + 206.43 * PA2 + 7856.91
