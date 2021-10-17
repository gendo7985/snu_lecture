# Exercise 
# 1. Write down the population mean and population "element-wise" variance of the random
# Wishart matrix. Use the Frobenius Norm on the different between the empirical and population 
# mean matrices (and variance matrices)  to record the deviation. (Google "Frobenius norm" for 
# definition.) What will happen to your deviation if $m$ increases (or $n$ increases)? 

Sigma <- matrix(c(10, 3, 1,
                  3, 6, 1,
                  1, 1, 6), nrow = 3)

m <- 100
M.array <- rWishart(n = 500, df = m, Sigma = Sigma)

mean.samp <- apply(M.array, c(1, 2), mean)
var.samp <- apply(M.array, c(1, 2), var)

mean.pop <- m * Sigma
d <- as.matrix(diag(Sigma))
var.pop <- m * (Sigma ^ 2 + d %*% t(d))

mean.dev <- norm(mean.samp - mean.pop, "F")
var.dev <- norm(var.samp - var.pop, "F")

# m increases
mean.pop <- Sigma
var.pop <- Sigma ^ 2 + d %*% t(d)

mean.devs <- c()
var.devs <- c()
M <- c(seq(10, 1000, by=10), seq(1100,10000,by=100))

for (m in M) {
  M.array <- rWishart(n = 500, df = m, Sigma = Sigma)
  mean.samp <- apply(M.array, c(1, 2), mean)
  var.samp <- apply(M.array, c(1, 2), var)
  mean.dev <- norm(mean.samp - m * mean.pop, "F")
  var.dev <- norm(var.samp - m * var.pop, "F")
  mean.devs <- c(mean.devs, mean.dev)
  var.devs <- c(var.devs, var.dev)
}
plot(M, mean.devs)
lines(M, sqrt(M), col="red")

plot(M, var.devs)
lines(M, sqrt(norm(var.pop, "F"))*M, col="red")

# n increases
mean.devs <- c()
var.devs <- c()
N <- M
m <- 100
for (n in N) {
  M.array <- rWishart(n = n, df = m, Sigma = Sigma)
  mean.samp <- apply(M.array, c(1, 2), mean)
  var.samp <- apply(M.array, c(1, 2), var)
  mean.dev <- norm(mean.samp - m * mean.pop, "F")
  var.dev <- norm(var.samp - m * var.pop, "F")
  mean.devs <- c(mean.devs, mean.dev)
  var.devs <- c(var.devs, var.dev)
}
plot(N, mean.devs)
abline(h=0, col="red")

plot(N, var.devs)
abline(h=0, col="red")

# 2. Verify the distribution of the LR statistic is indeed chi^2, using a qqplot.  
LRs <- c()
p <- 3
m <- 100
for (i in 1:100){
  M <- rWishart(n = 1, df = m, Sigma = diag(rep(1,p)))
  lambdas <- eigen(M[,,1] / m)$values
  LR <- m * (sum(lambdas) - p - sum(log(lambdas)))
  LRs <- c(LRs, LR)
}
y <- rchisq(100, df=6)
qqplot(LRs, y, main="QQplot")
qqline(y, distribution = function(p) qchisq(p, df = 6),
       probs = c(0.1, 0.6), col = 2)

# (IV) Exercises for inference on multivariate locations ----------------------------
# Textbook problem 5.9 (Demonstration)
n <- 61
X.bar <- c(95.52, 164.38, 55.69, 93.39, 17.98, 31.13)
S <- matrix(c(3266.46, 1343.97, 731.54, 1175.5, 162.68, 238.37,
              1343.97, 721.91, 324.25, 537.35, 80.17, 117.73,
              731.54, 324.25, 179.28, 281.17, 39.15, 56.8,
              1175.5, 537.35, 281.17, 474.98, 63.73, 94.85,
              162.68, 80.17, 39.15, 63.73, 9.95, 13.88,
              238.37, 117.73, 56.8, 94.85, 13.88, 21.26), nrow=6)

# (a) large sample 95% CI for six variables
len <- qt(0.05 / 2, df = n - 1, lower.tail = FALSE) * sqrt(diag(S/n))
CI.lower <- X.bar - len
CI.upper <- X.bar + len
(CI <- cbind(CI.lower, CI.upper))

# (b) large sample 95% CI ellipse for Weight, girth(1, 4 variable)
X.bar.2 <- X.bar[c(1, 4)]
S.2 <- S[c(1, 4), c(1, 4)]
S.eigen <- eigen(solve(S.2))
p <- 2
t <- qf(0.05, p, n-p, lower.tail=FALSE)*(n-1) * p / n / (n-p)
a <- sqrt(t / S.eigen$values[1])
b <- sqrt(t / S.eigen$values[2])
phi <- atan(S.eigen$vectors[2,1] / S.eigen$vectors[1,1])
i <- seq(0, 2*pi, 0.01)
xx <- X.bar.2[1] + a * cos(i)*cos(phi) - b*sin(i)*sin(phi)
yy <- X.bar.2[2] + a*cos(i)*cos(phi) + b*sin(i)*cos(phi)
plot(xx, yy, pch=19, col='red', xlim=c(70, 120), ylim=c(80, 105))

# (c) Bonferroni  CI for 6 variable
len <- qt(0.05 / 2 / 6, df = n-1, lower.tail=FALSE) * sqrt(diag(S/n))
CI.lower <- X.bar - len
CI.upper <- X.bar + len
(BCI <- cbind(CI.lower, CI.upper))

# (d) Bonferroni vs Ellipse
rect(BCI[1,1],BCI[4,1],BCI[1,2],BCI[4,2], border="blue")

# (e) Y = head width - head length
Y.bar <- X.bar[6] - X.bar[5]
Y.S <- S[5,5] - 2*S[5,6] + S[6,6]
len <- qt(0.05 / 2 / 7, df = n-1, lower.tail=FALSE) * sqrt(Y.S / n)
CI.lower <- Y.bar - len
CI.upper <- Y.bar + len
(cbind(CI.lower, CI.upper))

# 3. Continued from Problem 5.9. Test whether means of the variables are (100, 150, 50, 90, 17, 31). 
# Provide a p-value. 
mu0 <- c(100, 150, 50, 90, 17, 31)
p <- 6
T2 <- (n-p) / p / (n-1) * t(as.matrix(X.bar - mu0)) %*% solve(S) %*% as.matrix(X.bar - mu0)
(Fprob <- df(T2, p, n-p)) # 0.081
