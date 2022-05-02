##(A) Smoothing by using the Nadaraya-Watson estimator 
# (1)
d1 <- read.table("TEST31.CSV", sep = ",")
xx <- d1[, 1]
yy <- d1[, 2]
# (2)
nd <- length(xx)
# (3)
bw <- 0.18
# (4)
ex <- seq(from = 0.1, to = 3, by = 0.1)
# (5)
bwsplus <- bw/0.3708159
fit.ks <- ksmooth(xx, yy, kernel = "normal", 
                  bandwidth = bwsplus, x.points = ex)
# (6)
ey <- fit.ks$y
# (7) Figure 3.10
plot(xx, yy, type = "n", xlab = "X", ylab = "Y")
points(xx, yy, pch = 5, cex = 1.5, col = 'red')
lines(ex, ey, lwd = 3)


##(B) Calculation of CV and GCV of smoothing by Nadaraya-Watson estimator
kscv1 <- function(x1, y1, nd, bandw, ntrial){
# (1)
  kscvgcv <- function(bw, x1, y1)
  {
# (2)
    nd <- length(x1)
    bwsplus <- bw/0.3708159
    fit.ks <- ksmooth(x1, y1, "normal", bandwidth = bwsplus, n.points = nd)
    res <- y1 - fit.ks$y
# (3)
    dhat1 <- function(x2, bw)
    {
      nd2 <- length(x2)
      diag1 <- diag(nd2)
      bwsplus <- bw/0.3708159
      dhat <- rep(0, length = nd2)
# (4)
      for(jj in 1:nd2) {
        y2 <- diag1[, jj]
        fit.ks <- ksmooth(x2, y2, "normal", bandwidth = bwsplus, n.points = nd2)
        dhat[jj] <- fit.ks$y[jj]
      }
      return(dhat)
    }
# (5)
    dhat <- dhat1(x1, bw)
    trhat <- sum(dhat)
    sse <- sum(res^2)
# (6)
    cv <- sum((res/(1- dhat))^2)/nd
    gcv <- sse/(nd * (1- (trhat/nd))^2)
# (7)
    return(list(cv = cv, gcv = gcv))
  }
# (8)
  cvgcv <- lapply(as.list(bandw), kscvgcv,
                  x1 = x1, y1 = y1)
  cvgcv <- unlist(cvgcv)
  cv <- cvgcv[attr(cvgcv, "names") == "cv"]
  gcv <- cvgcv[attr(cvgcv, "names") == "gcv"]
# (9)
  return(list(cv = cv, gcv = gcv))
}

### Example of using kscv1 and Figure 3.11
# (1) 
mm <- read.table("TEST31.csv", sep = ",")
xx <- mm[[1]]
yy <- mm[[2]]
# (2)
nd <- length(xx)
# (3)
ntrial <- 10
# (4)
bandw <- seq(from = 0.03, by = 0.03, length = ntrial)
# (5)
output.ks <- kscv1(xx, yy, nd, bandw, ntrial)
cv <- output.ks$cv
gcv <- output.ks$gcv
# (6) Figure 3.11 (left)
par(mfrow = c(1, 2))
plot(bandw, cv, type = "n", ylim = c(0.04, 0.07),
     xlab = "bandwidth", ylab = "CV")
points(bandw, cv, cex = 1.2, pch = 4)
lines(bandw, cv, lwd = 2)
# (7)
cvmin <- min(cv)
# (8)
icvmin <- (1.:ntrial)[cv == cvmin]
bandcv <- bandw[icvmin]
points(bandcv, cvmin, cex = 1.2, pch = 15)
# (9) Figure 3.11 (right)
plot(bandw, gcv, type = "n", ylim = c(0.04, 0.07),
     xlab = "bandwidth", ylab = "GCV")
points(bandw, gcv, cex = 1.2, pch = 4)
lines(bandw, gcv, lwd = 2)
# (10)
gcvmin <- min(gcv)
# (11)
igcvmin <- (1.:ntrial)[gcv == gcvmin]
bandgcv <- bandw[igcvmin]
points(bandgcv, gcvmin, cex = 1.2, pch = 15)


##(C) Calculation of CV and GCV in the case of 
### smoothing by local linear regression
library(mgcv)
llcv1 <- function(x1, y1, nd, bandw, ntrial)
{
# (1)
  llinest <- function(ex1, xdata, ydata, band)
  {
# (2)
    x2 <- xdata - ex1
    wts <- exp((-0.5 * x2^2)/band^2)
# (3)
    data1 <- data.frame(x = x2, y = ydata, www = wts)
# (4)
    fit.lm <- lm(y ~ x, data = data1, weights = www)
# (5)
    est <- fit.lm$coef[1]
    names(est) <- NULL
# (6)
    return(est)
  }  
# (7)
  dhat2 <- function(xdata, band)
  {
# (8)
    dhat3 <- function(ydata, xdata, band)
    {                   
# (9)
      x2 <- xdata - xdata[ydata == 1]
      wts <- exp((-0.5 * x2^2)/band^2)
      data1 <- data.frame(x = x2, y = ydata, www = wts)
# (10)
      fit.lm <- lm(y ~ x, data = data1, weights = www,
                   x = T)
      inf1 <- lm.influence(fit.lm)
      dhatj <- inf1$hat[ydata == 1]
# (11)
      return(dhatj)
    }
# (12)
    diag1 <- diag(length(xdata))
# (13)
    dhat <- apply(diag1, 2, dhat3, xdata = xdata,
                  band = band)
    dhat <- as.vector(dhat)
# (14)
    return(dhat)
  }
# (15)
  cv <- numeric(ntrial)
  gcv <- numeric(ntrial)
# (16)
  for(kk in 1.:ntrial) {
# (17)
    ey <- lapply(as.list(x1), llinest, xdata = x1, 
                 ydata = y1, band = bandw[kk])
    ey <- unlist(ey)
# (18)
    res <- y1 - ey
    sse <- sum(res^2)
# (19)
    dhat <- dhat2(x1, bandw[kk])
    trhat <- sum(dhat)
# (20)
    cv[kk] <- sum((res/(1 - dhat))^2)/nd
    gcv[kk] <- sse/(nd * (1 - (trhat/nd))^2)
  }
# (21)
  return(list(cv = cv, gcv = gcv))
}

### Example of using llcv1 and Figure 3.18
# (1)
mm <- read.table("TEST31.csv", sep = ",")
xx <- mm[[1]]
yy <- mm[[2]]
# (2)
nd <- length(xx)
# (3)
ntrial <- 10
# (4)
bandw <- seq(from = 0.1, by = 0.05, length = ntrial)
# (5)
output.ll <- llcv1(xx, yy, nd, bandw, ntrial)
cv <- output.ll$cv
gcv <- output.ll$gcv
# (6) Figure 3.18 (left)
par(mfrow = c(1, 2))
plot(bandw, cv, type = "n", ylim = c(0.04, 0.07),
     xlab = "bandwidth", ylab = "CV")
points(bandw, cv, cex = 1.2, pch = 4)
lines(bandw, cv, lwd = 2)
# (7)
cvmin <- min(cv)
# (8)
icvmin <- (1.:ntrial)[cv == cvmin]
bandcv <- bandw[icvmin]
points(bandcv, cvmin, cex = 1.2, pch = 15)
# (9) Figure 3.19 (right)
plot(bandw, gcv, type = "n", ylim = c(0.04, 0.07),
      xlab = "bandwidth", ylab = "GCV")
points(bandw, gcv, cex = 1.2, pch = 4)
lines(bandw, gcv, lwd = 2)
# (10)
gcvmin <- min(gcv)
# (11)
igcvmin <- (1:ntrial)[gcv == gcvmin]
bandgcv <- bandw[igcvmin]
points(bandgcv, gcvmin, cex = 1.2, pch = 15)


##(D) Smoothing by local linear regression
ll1 <- function(ex1, xdata, ydata, band)
{
# (1)
  x2 <- xdata - ex1
  wts <- exp((-0.5 * x2^2)/band^2)
# (2)
  data1 <- data.frame(x = x2, y = ydata, www = wts)
# (3)
  fit.lm <- lm(y ~ x, data = data1, weights = www)
# (4)
  est <- fit.lm$coef[1.]
  names(est) <- NULL
# (5)
  return(est)
}


### Example of an object to use ll1() and Figure 3.19
# (1)
mm <- read.table("TEST31.csv", sep = ",")
xx <- mm[[1]]
yy <- mm[[2]]
# (2)
band1 <- 0.25
# (3)
ne <- 59
# (4)
ex <- seq(from = 0.1, to = 3, length = ne)
ey <- sapply(as.list(ex), ll1, xdata = xx, ydata = yy,
              band = band1)
# (5)
plot(xx, yy, type = "n", xlab = "x", ylab = "y")
points(xx, yy, cex = 1.5, pch = 5, col = "red")
lines(ex, ey, lwd = 3)
  

##(E) Interpolation using B-spline
library(splines)
intsp1 <- function(x1, y1, intkn, ex)
{
# (1)
  data1 <- data.frame(x = x1, y = y1)
  assign("data1", data1)
  formula.name <- substitute(y ~ bs(x, knots = kn,
          degree = 3), list(kn = intkn))
  fit.lm <- lm(formula.name, data = data1)
# (2)
  data2 <- data.frame(x = ex)
  ey <- predict(fit.lm, newdata = data2)
# (3)
  return(ey)
}
  

### Example of using intsp1 and Figure 3.25
# (1) 
xx <- c(1, 1.7, 3, 4.2, 5)
yy <- c(2, 5, 4, 5, 3)
intkn <- c(3.5)
# (2)
ne <- 41
ex <- seq(from = 1, by = 0.1, length = ne)
# (3)
ey <- intsp1(xx, yy, intkn, ex)
# (4)
plot(ex, ey, type = "n", xlab = "x", ylab = "y",
      ylim = c(0, 6))
points(xx, yy, pch = 4, cex = 2, col = "red", lwd = 2)
lines(ex, ey, lwd = 2)
# (5)
points(c(min(xx), intkn, max(xx)), rep(0.9,
    length = length(intkn) + 2), pch = 15, cex = 1.5)
  

##(F) Interpolation using natural spline
library(stats)
library(splines)
#use of spline()
intns1 <- function(x1, y1, ne)
{
# (1)
  exy <- spline(x1, y1, n = ne, method = 'natural')
# (2)
  ex <- exy$x
  ey <- exy$y
# (3)
  return(list(ex = ex, ey = ey))
}

#Recall chapter2 : (C) Smoothing by the binomial filter
binom1<-function(yy, mm)
{
  #(1)
  nd <- length(yy)
  #(2)
  mm2 <- mm * 0.5
  #(3)
  yywl <- yy
  yyw2 <- yy
  rlim <- mm2
  yyr <- NULL
  count <- 0
  while(rlim > nd) {
    yyw1 <- rev(yyw1)
    yyr <- c(yyr, yyw1)
    rlim <- rlim - nd
    count <- count + 1
  }
  switch(count %% 2 + 1,
         yyr <- c(yyr, yy[nd:(nd - rlim + 1)]),
         yyr <- c(yyr, yy[1:rlim]))
  llim <- mm2
  yyl <- NULL
  while (llim > nd) {
    yyw2 <- rev(yyw2)
    yyl <- c(yyw2, yyl)
    llim <- llim - nd
  }
  switch(count %% 2 + 1,
         yyl <- c(yy[llim:1], yyl),
         yyl <- c(yy[(nd - llim + 1):nd], yyl))
  y2 <- matrix(c(yyl, yy, yyr), ncol = 1)
  #(4)
  ww <- matrix(0, ncol = nd + mm, nrow = nd + mm)
  #(5)
  imat <- row(ww)
  jmat <- col(ww)
  #(6)
  check <- 0 <= (mm2 + imat - jmat) & (mm2 + imat - jmat) <= mm
  #(7)
  ww[check] <- exp(lgamma(mm +1) -
                     lgamma(mm2 + imat[check] - jmat[check] + 1) -
                     lgamma(mm2 - imat[check] + jmat[check] + 1) -
                     mm * logb(2))
  #(8)
  ey <- ww %*% y2
  ey <- as.vector(ey[(mm2 + 1):(nd + mm2)])
  #(9)
  return(ey)
}

### Example of using intns1 and Figure 3.30
# (1)
yd <- scan("WAK2.csv")
yy <- binom1(yd, 2) ## From Chapter 2
nd <- length(yy)
xx <- seq(from = 1, by = 1, length = nd)
# (2)
ne <- 301
# (3)
exy <- intns1(xx, yy, ne)
ex <- exy$ex
ey <- exy$ey
# (4)
plot(xx, yy, type = "n", ylim = c(-15, 5), xlab = "X",
     ylab = "Y", cex.lab = 1.5)
lines(ex, ey, lwd = 2)
points(xx, yy, pch = 15, col = 'red')
points(xx, yd, pch = 4, col = 'blue')

#use of smooth.spline() with very small lambda
intns2 <-function(x1, y1, ex)
{
# (1)
  fit.sp <- smooth.spline(x1, y1, lambda = 1e-010, all.knot = T)
# (2)
  exy <- predict(fit.sp, ex)
# (3)
  ey <- exy$y
# (4)
  return(ey)
}

### example of using intns2 plot similar to Figure 3.30
# (3)
ex <- seq(from = 1, length = ne, by = 0.1)
ey <- intns2(xx, yy, ex)
# (4)
plot(xx, yy, type = "n", ylim = c(-15, 5), xlab = "X",
     ylab = "Y", cex.lab=1.5)
lines(ex, ey, lwd = 2)
points(xx, yy, pch = 15, col='red')
points(xx, yd, pch = 4, col='blue')

### use of lm() and ns() to to spline interpolation
intns3 <- function(x1, y1, ex)
{
# (1)
  data1 <- data.frame(x = x1, y = y1)
# (2)
  kn1 <- x1[2:(length(x1) - 1)]
# (3)
  formula.name <- substitute(y ~ ns(x, knots = kn),
                             list(kn = kn1))
  fit.lm <- lm(formula.name, data = data1)
# (4)
  data2 <- data.frame(x = ex)
  ey <- predict(fit.lm, newdata = data2)
# (5)
  return(ey)
}
  
### example of using intns3 plot similar to Figure 3.30
# (3)
ex <- seq(from = 1, length = ne, by = 0.1)
ey <- intns3(xx, yy, ex)
# (4)
plot(xx, yy, type = "n", ylim = c(-15, 5), xlab = "X",
      ylab = "Y")
lines(ex, ey, lwd = 2)
points(xx, yy, pch = 15, col = "red")
points(xx, yd, pch = 4, col = "blue")


###(H) Code for calculating CV and GCV for 
### smoothing spline and Figures 3.38, 3.39
# (1)
ll <- list(r1 = 0, r2 = 0)
mm <- scan("TEST31.csv", ll, sep = ",")
xx <- mm$r1
yy <- mm$r2
nd <- length(xx)
# (2)
ntrial <- 46
lambdavec <- 10^seq(from = -4, by = 0.1, length = ntrial)
cv <- rep(0, length = ntrial)
gcv <- rep(0, length = ntrial)
# (3)
for(k in c(1:ntrial)) {
# (4)
  fit.sp <- smooth.spline(xx, yy, cv = T, lambda = lambdavec[k], all.knots = T)
# (5)
  cv[k] <- fit.sp$cv
  
  fit.sp <- smooth.spline(xx, yy, cv = F, lambda = lambdavec[k], all.knots = T)
  gcv[k] <- fit.sp$cv
  
}
# (6) CV
par(mfrow = c(1, 2), mar = c(3, 4, 2, 1),oma=c(0.5,0.5,0.5,0.5))

plot(log10(lambdavec), cv, ylim = c(0.04, 0.2),
    xlab = "log10(lambda)", ylab = "CV", type = "n")
points(log10(lambdavec), cv, pch = 3)
lines(log10(lambdavec), cv, lwd = 2)
cvmin <- min(cv)
icvmin <- (1:ntrial)[cv == cvmin]
lambdacv <- lambdavec[icvmin]
points(log10(lambdavec[icvmin]), cvmin, pch = 15)
# (7)
fit.sp <- smooth.spline(xx, yy, lambda=lambdavec[icvmin],
                        all.knot = T)
ex <- seq(from = 0.1, to = 3, by = 0.02)
exy <- predict(fit.sp, ex)
ey <- exy$y
plot(xx, yy, xlab = "x", ylab = "y", type = "n")
points(xx, yy, pch = 5)
lines(ex, ey, lwd = 2)

# GCV
par(mfrow = c(1, 2), mar = c(3, 4, 2, 1),
    oma=c(0.5,0.5,0.5,0.5), cex.lab=1.5)
plot(log10(lambdavec), gcv, ylim = c(0.04, 0.2),
     xlab = "log10(lambda)", ylab = "GCV", type = "n")
points(log10(lambdavec), gcv, pch = 3)
lines(log10(lambdavec), gcv, lwd = 2)
gcvmin <- min(gcv)
igcvmin <- (1:ntrial)[gcv == gcvmin]
lambdagcv <- lambdavec[igcvmin]
points(log10(lambdavec[icvmin]), cvmin, pch = 15)
# (7)
fit.sp <- smooth.spline(xx, yy, lambda=lambdavec[igcvmin],
                        all.knot = T)
ex <- seq(from = 0.1, to = 3, by = 0.02)
exy <- predict(fit.sp, ex)
ey <- exy$y
plot(xx, yy, xlab = "x", ylab = "y", type = "n")
points(xx, yy, pch = 5)
lines(ex, ey, lwd = 2)



##(I) Calculation of CV and GCV for LOESS
locv1 <- function(x1, y1, nd, span1, ntrial)
{
# (1)
  locvgcv <- function(sp, x1, y1)
  {
    nd <- length(x1)
# (2)
    assign("data1", data.frame(xx1 = x1, yy1 = y1))
    fit.lo <- loess(yy1 ~ xx1, data = data1, span = sp,
                    family = "gaussian", degree = 1, surface = "direct")
    res <- residuals(fit.lo)
# (3)
    dhat2 <- function(x1, sp)
    {
      nd2 <- length(x1)
      diag1 <- diag(nd2)
      dhat <- rep(0, length = nd2)
# (4)
      for(jj in 1:nd2) {
        y2 <- diag1[, jj]
        assign("data1", data.frame(xx1 = x1, yy1 = y2))
        fit.lo <- loess(yy1 ~ xx1, data = data1,
                        span = sp, family = "gaussian", degree = 1,
                        surface = "direct")
        ey <- fitted.values(fit.lo)
        dhat[jj] <- ey[jj]
      }
      return(dhat)
    }
# (5)
    dhat <- dhat2(x1, sp)
    trhat <- sum(dhat)
    sse <- sum(res^2)
# (6)
    cv <- sum((res/(1 - dhat))^2)/nd
    gcv <- sse/(nd * (1 - (trhat/nd))^2)
# (7)
    return(list(cv = cv, gcv = gcv))
  }
# (8)
  cvgcv <- lapply(as.list(span1), locvgcv, x1 = x1, y1 = y1)
  cvgcv <- unlist(cvgcv)
  cv <- cvgcv[attr(cvgcv, "names") == "cv"]
  gcv <- cvgcv[attr(cvgcv, "names") == "gcv"]
# (9)
  return(list(cv = cv, gcv = gcv))
}

### Figure Similar to Figure 3.45
# (1)
set.seed(195)
nd <- 40
xx <- seq(from = 1, by = 1, length = nd)^1.8
yy <- sin(0.004 * pi * xx) + rnorm(nd, mean = 0, sd = 0.3)
# (2)
ntrial <- 10
span1 <- seq(from = 0.15, by = 0.01, length = ntrial)
# (3)
output.lo <- locv1(xx, yy, nd, span1, ntrial)
cv <- output.lo$cv
gcv <- output.lo$gcv
# (4)
par(mfrow = c(1, 2), mar = c(3, 4, 2, 1),
    oma=c(0.5,0.5,0.5,0.5), cex.lab=1.5)
plot(span1, cv, type = "n", 
     xlab = "span", ylab = "CV")
points(span1, cv, pch = 3)
lines(span1, cv, lwd = 2)
pcvmin <- seq(along = cv)[cv == min(cv)]
spancv <- span1[pcvmin]
cvmin <- cv[pcvmin]
points(spancv, cvmin, cex = 1, pch = 15)
# (5)
plot(span1, gcv, type = "n", 
     xlab = "span", ylab = "GCV")
points(span1, gcv, pch =4)
lines(span1, gcv, lwd = 2)
pgcvmin <- seq(along = gcv)[gcv == min(gcv)]
spangcv <- span1[pgcvmin]
gcvmin <- gcv[pgcvmin]
points(spangcv, gcvmin, cex = 1, pch = 15)


##(J) Smoothing by supersmoother(use of supsmu()) and Figure 3.46
# (1)
nd <- 100
set.seed(123)
xx <- seq(from = 1, by = 1, length = nd)
yy <- sin(0.04 * pi * xx) + rnorm(nd, mean = 0,
                                  sd = 0.02) * xx
# (2)
par(mfrow = c(2, 2), mar = c(3, 4, 2, 1),
    oma=c(0.5,0.5,0.5,0.5), cex.lab=1.5)
# (3)
fit.su <- supsmu(xx, yy, span = 0.05)
plot(xx, yy, type = "n", xlab = "x", ylab = "y (tweeter)")
points(xx, yy, pch = 15, cex = 0.3)
lines(fit.su$x, fit.su$y, lwd = 2)
# (4)
fit.su <- supsmu(xx, yy, span = 0.2)
plot(xx, yy, type = "n", xlab = "x", ylab = "y (midrange)")
points(xx, yy, pch = 15, cex = 0.3)
lines(fit.su$x, fit.su$y, lwd = 2)
# (5)
fit.su <- supsmu(xx, yy, span = 0.5)
plot(xx, yy, type = "n", xlab = "x", ylab = "y (woofer)")
points(xx, yy, pch = 15, cex = 0.3)
lines(fit.su$x, fit.su$y, lwd = 2)
# (6)
fit.su <- supsmu(xx, yy, span = "cv")
plot(xx, yy, type = "n", xlab = "x", ylab =
       "y (supersmoother)")
points(xx, yy, pch = 15, cex = 0.3)
lines(fit.su$x, fit.su$y, lwd = 2)

 
##(K) Smoothing by LOWESS (robust version of LOESS) 
### and by LOESS (use of loess()) and Figure 3.49
# (1)
nd <- 100
set.seed(123)
xx <- seq(from = 1, by = 1, length = nd)
yy <- sin(0.04 * pi * xx) + rnorm(nd, mean = 0, sd = 0.5)
yy[30] <- 5
yy[50] <- 10
yy[70] <- 5
# (2)
ex <- seq(from = 1, to = nd, by = 0.1)
# (3)
assign("data1", data.frame(x = xx, y = yy))
data2 <- data.frame(x = ex)
# (4)
par(mfrow = c(2, 2), mar = c(3, 4, 2, 1),
    oma=c(0.5,0.5,0.5,0.5), cex.lab=1.5)
# (5)
fit.low <- lowess(xx, yy, f= 0.1)
plot(xx, yy, type = "n", xlab = "x",
     ylab = "y (LOWESS, span=0.1)")
points(xx, yy, pch = 1, cex = 0.3)
lines(fit.low, lwd = 2)
# (6)
fit.low <- lowess(xx, yy, f= 0.2)
plot(xx, yy, type = "n", xlab = "x",
     ylab = "y(LOWESS, span=0.2)")
points(xx, yy, pch = 15, cex = 0.3)
lines(fit.low, lwd = 2)
# (7)
fit.lo <- loess(y ~ x, data = data1, span = 0.1,
                family = "gaussian", degree = 1, surface = "direct")
ey <- predict(fit.lo, newdata = data2)
plot(xx, yy, type = "n", xlab = "x",
     ylab = "y (LOESS, span=0.1)")
points(xx, yy, pch = 15, cex = 0.3)
lines(ex, ey, lwd = 2)
# (8)
fit.lo <- loess(y ~ x, data = data1, span = 0.2,
                family = "gaussian", degree = 1, surface = "direct")
ey <- predict(fit.lo, newdata = data2)
plot(xx, yy, type = "n", xlab = "x",
     ylab = "y, (LOESS, span=0.2)")
points(xx, yy, pch = 1, cex = 0.3)
lines(ex, ey, lwd = 2)
