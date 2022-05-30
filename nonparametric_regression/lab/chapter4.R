##(A) Calculation of CV and GCV for local linear regression with two predictors
llin2dcv<-function(h12, nd, xx1, xx2, yy)
{
# (1)
  influence1 <- function(ii, xx1, xx2, yy, h1, h2)
  {
# (2)
    cpx1 <- xx1[ii]
    cpx2 <- xx2[ii]
# (3)
    wts <- exp(-0.5 * ((xx1 - cpx1)/h1)^2 - 0.5 *
      ((xx2 - cpx2)/h2)^2)
# (4)
    data1 <- data.frame(x1 = xx1 - cpx1, x2 = xx2 - cpx2,
      y = yy, www = wts)
    fit.lm <- lm(y ~ x1 + x2, data = data1,
      weights = www, x = T)
# (5)
    res <- yy[ii] - fit.lm$coef[1]
    names(res) <- NULL
    hat1 <- lm.influence(fit.lm)$hat[ii]
    names(hat1)<- NULL
    return(list(res = res, hat1 = hat1))
  }
# (6)
  influence1.out <- lapply(as.list(c(1:nd)),
    influence1, xx1 = xx1, xx2 = xx2, yy = yy, h1 =
    h12[1], h2 = h12[2])
# (7)
  resdhat <- unlist(influence1.out)
  res <- resdhat[attr(resdhat, "names") == "res"]
  dhat <- resdhat[attr(resdhat, "names") == "hat1"]
# (8)
  trhat <- sum(dhat)
  sse <- sum(res^2)
  cv <- sum((res/(1 - dhat))^2)/nd
  gcv <- sse/(nd * (1 - (trhat/nd))^2)
# (9)
  return(list(cv = cv, gcv = gcv))
}

## An example of an object to use llin2dcv() 
## Construction of figure 4.1
# (1)
nd <- 100
set.seed(2525)
xx1 <- runif(nd, min = 0, max = 10)
xx2 <- runif(nd, min = 0, max = 10)
yy <- (xx1 - 4)^2 + (xx2 - 6)^2 + rnorm(nd,
  mean =0, sd = 3)
# (2)
ntrialx2 <- 10
ntrialx1 <- 10
hx1 <- seq(from = 0.5, to = 1.4, length = ntrialx1)
hx2 <- seq(from = 0.5, to = 1.4, length = ntrialx2)
hx12 <- expand.grid(hx1, hx2)
listhx12 <- list(unlist(hx12[1, ]))
for(ii in 2:(ntrialx1 * ntrialx2)) {
listhx12[[ii]] <- unlist(hx12[ii, ])
}
# (3)
cvgcv <- lapply(listhx12, llin2dcv, nd = nd,
  xx1 = xx1, xx2 = xx2, yy = yy)
# cvgcv <- apply(hx12, 1, llin2dcv, nd = nd, xx1 = xx1, xx2 = xx2, yy = yy)
cvgcv <- unlist(cvgcv)
cvt <- cvgcv[attr(cvgcv, "names") == "cv"]
gcvt <- cvgcv[attr(cvgcv, "names") == "gcv"]
# (4)
gcvmin <- min(gcvt)
igcvmin <- (1:(ntrialx1 * ntrialx2))[gcvt == gcvmin]
print("optimal bandwidth")
print(hx12[igcvmin, 1])
print(hx12[igcvmin, 2])
# (5)
par(mfrow = c(1, 2))
contour(hx1, hx2, matrix(cvt, nrow = ntrialx1), levels = 
  seq(from = 0, to = 30, by = 0.5), xlab = "h1",
  ylab = "h2", main = "CV")
contour(hx1, hx2, matrix(gcvt, nrow = ntrialx1), levels =
  seq(from = 0, to = 30, by = 0.5), xlab = "h1",
  ylab = "h2", main = "GCV")


##(B) Local linear regression with two predictors

llin2d<-function(ex, nd, xx1, xx2, yy, hx1, hx2)
{  
# (1) 
  wts <- exp(-0.5 * ((xx1 - ex[1])/hx1)^2 - 0.5 *
    ((xx2 - ex[2])/hx2)^2)
# (2)
  data1 <- data.frame(x1 = xx1 - ex[1], x2 = xx2 - ex[2],
    y = yy, www= wts)
# (3)
  fit.lm <- lm(y ~ x1 + x2, data = data1, weights = www)
# (4)
  est <- fit.lm$coef[1]
# (5)
  return(est)
}

## An example of an object to use llin2d()
## Construction of figure 4.2
# (1)
nd <- 100
set.seed(2525)
xx1 <- runif(nd, min = 0, max = 10)
xx2 <- runif(nd, min =0, max = 10)
yy <- (xx1 - 4)^2 + (xx2 - 6)^2 + rnorm(nd,
  mean = 0, sd = 3)
# (2)
hx1 <- 0.7
hx2 <- 0.8
# (3)
nex1 <- 11
gridx1 <- seq(from = 0, to = 10, length = nex1)
nex2 <- 21
gridx2 <- seq(from = 0, to = 10, length = nex2)
grid <- expand.grid(gridx1, gridx2)
# (4)
ey <- apply(grid, 1, llin2d, nd = nd, xx1 = xx1,
  xx2 = xx2, yy = yy, hx1 = hx1, hx2 = hx2)
# (5)
par(mfrow = c(1, 2))
contour(gridx1, gridx2, matrix(ey, nrow = nex1), levels = 
  seq(from =0, to = 100, by = 5),  xlab = "x1",
  ylab = "x2")
# (6)
library(interp)
xyz.in <- interp(xx1, xx2, yy)
contour(xyz.in, levels = seq(from =0, to = 100, by = 5), xlab = "x1", ylab = "x2")

  
##(C) Smoothing using the thin plate smoothing splines 
## for data with two predictors

# (1)
nobs <- 100
set.seed(2525)
xx1 <- runif(nobs, min =0, max = 10)
xx2 <- runif(nobs, min = 0, max = 10)
yy <- (xx1 - 4)^2 + (xx2 - 6)^2 + rnorm(nobs, mean = 0,
  sd = 3)
# (2)
des <- cbind(xx1, xx2)
y <- yy
# (3)
library(fields)
fit <-Tps(des, y)
# out.p <- predictSurface(fit)
# surface(out.p, type = "C")
xyz.in <- interp(xx1, xx2, fit$fitted.values)
contour(xyz.in, levels = seq(from =0, to = 100, by = 5), xlab = "x1", ylab = "x2")


  
##(D) Smoothing by LOESS with two predictors
# (1)
nd <- 100
set.seed(2525)
xx1 <- runif(nd, min = 0, max = 10)
xx2 <- runif(nd, min = 0, max = 10)
yy <- (xx1 - 4)^2 + (xx2 - 6)^2 + rnorm(nd,
  mean = 0, sd = 3)
# (2)
nex1 <- 11
nex2 <- 11
gridx1 <- seq(from = 0, by = 1, length = nex1)
gridx2 <- seq(from = 0, by = 1, length = nex2)
grid <- expand.grid(gridx1, gridx2)
assign("data1", data.frame(x1 = xx1, x2 = xx2, y1 = yy)) 

# (3)
par(mfrow = c(1, 2))
# (4)
sp1 <- 0.1
fit.lo <- loess(y1 ~ x1 + x2, data = data1,
  span = sp1, family = "gaussian", degree = 1,
  surface = "direct")
data2 <- data.frame(x1 = grid[, 1], x2 = grid[, 2])
ey <- predict(fit.lo, newdata = data2)
# (5)
grid.es <- NULL
grid.es$x <- gridx1
grid.es$y <- gridx2
grid.es$z <- matrix(ey, nrow = nex1)
contour(grid.es, levels = seq(from =0, to = 100, by = 5),
  xlab = "x1", ylab = "x2")
# (6)
sp1 <- 0.2
fit.lo <- loess(y1 ~ x1 + x2, data = data1,
  span = sp1, family = "gaussian", degree = 1,
  surface = "direct")
data2 <- data.frame(x1 = grid[, 1], x2 = grid[, 2])
ey <- predict(fit.lo, newdata = data2)
print(ey)
grid.es <- NULL
grid.es$x <- gridx1
grid.es$y <- gridx2
grid.es$z <- matrix(ey, nrow = nex1)
contour(grid.es, levels = seq(from =0, to = 100, by = 5),
  xlab = "x1", ylab = "x2")

  
##(E) Ordinary kriging with one predictor
mykrig1<-function(nd, ne, xx, yy, ex, bw)
{
# (1)
  full <- matrix(0, nd, nd)
  full[lower.tri(full)] <- dist(xx)
  dist1 <- full + t(full)
  cc <- exp( - dist1^2/bw^2)
# (2)
  dvector <- function(exs, xx, bw)
  {
    dv <- exp( - (exs - xx)^2/bw^2)
    return(dv)
  }
  dmat <- apply(matrix(ex, nrow = 1), 2, dvector, xx = xx,
    bw = bw)
  # dim(dmat)
# (3)
  ccr <- solve(cc)
  ccs <- sum(apply(ccr, 2, sum)) # same as "sum(ccr)"
  chi <- (-2 * (1 - apply((ccr %*% dmat), 2, sum)))/ccs
  chimat <- matrix(rep(chi, times = nd), ncol = ne,
    byrow = T)
# (4)
  ww <- ccr %*% (dmat - 0.5 * chimat)
  ey <- t(ww) %*% yy
# (5)
  return(ey)
}
  
## An example of an object to use mykrig1()
## Construction of figure 4.5
# (1)
nd <- 20
set.seed(123)
xx <- seq(from = 1 , by = 1, length = nd)
yy <- sin(0.1 * pi * xx) + 1 + 
  rnorm(nd, mean =0, sd = 0.5)
# (2)
ne <- 96
ex <- seq(from = 1, by = 0.2, length = ne)
band <- c(0.3, 1, 3)
# (3)
par(mfrow = c(2, 2))
# (4)
for(jj in 1 :3) { 
# (5)
  bw <- band[jj]
  ey <- mykrig1(nd, ne, xx, yy, ex, bw)
# (6)
  plot(xx, yy, type = "n", xlab = "x",
    ylab = "y", ylim = c(-1, 3))
  points(xx, yy, cex = 1.2, pch =2)
  lines(ex, ey, lwd =2)
} 
exy <- spline(xx, yy, n = ne)
ey <- exy$y
# (8)
plot(xx, yy, type = "n", xlab = "x",
  ylab = "y", ylim = c(-1, 3))
points(xx, yy, cex = 1.2, pch =2)
lines(ex, ey, lwd =2)

  
##(F) Smoothing by simple kriging with one predictor
mykrigs<-function(nd, ne, xx, yy, ex, bw, sig2)
{
# (1)
  full <- matrix(0, nd, nd)
  full[lower.tri(full)] <- dist(xx)
  dist1 <- full + t(full)
  cc <- exp( - dist1^2/bw^2)
# (2)
  dvector <- function(exs, xx, bw)
  {
    dv <- exp( - (exs - xx)^2/bw^2)
    return(dv)
  }
  dmat <- apply(matrix(ex, nrow = 1),
    2, dvector, xx = xx, bw = bw)
# (3)
  ccr <- solve(cc + sig2 * diag(nd))
  # ccs <- sum(apply(ccr, 2, sum))
# (4)
  ww <- ccr %*% dmat
  ey <- t(ww) %*% yy
# (5)
  return(ey)
}

## An example of an object to use mykrigs()
## Construction of figure 4.7
# (1)
nd <- 20
set.seed(123)
xx <- seq(from = 1 , by = 1, length = nd)
yy <- sin(0.1 * pi * xx) + 1 + 
  rnorm(nd, mean =0, sd = 0.5)
# (2)
ne <- 96
ex <- seq(from = 1, by = 0.2, length = ne)
band <- c(0.1, 0.3, 1, 3)
sig2 = 0.25
# (3)
par(mfrow = c(2, 2))
# (4)
for(jj in 1:4) { 
  # (5)
  bw <- band[jj]
  ey <- mykrigs(nd, ne, xx, yy, ex, bw, sig2)
  # (6)
  plot(xx, yy, type = "n", xlab = "x",
       ylab = "y", ylim = c(-1, 3), main = paste0("delta : ", bw))
  points(xx, yy, cex = 1.2, pch =2)
  lines(ex, ey, lwd =2)
} 


##(G) Universal kriging with one predictor
mykrig2<-function(nd, ne, xx, yy, ex, bw, np)
{
# (1) 
  full <- matrix(0, nd, nd)
  full[lower.tri(full)] <- dist(xx)
  dist1 <- full + t(full)
  cc <- exp( - dist1^2/bw^2)
# (2)
  ll <- t(chol(cc))
  llr <- solve(ll)
# (3)
  powerf <- function(jj, x1)
  {
    pw <- x1^jj
    return(pw)
  }
  gg <- apply(matrix(c(0:(np - 1)), nrow = 1), 2, powerf,
    x1 = xx)
  aa <- qr(llr %*% gg)
  qq <- qr.Q(aa)
  rr <- qr.R(aa)
# (4)
  bb <- t(qq) %*% llr %*% yy
  beta1 <- solve(rr[1:np, ], bb[1:np])
# (5)
  dvector <- function(exs, xx, bw)
  {
    dv <- exp( - (exs - xx)^2/bw^2)
    return(dv)
  }
  dmat <- apply(matrix(ex, nrow = 1), 2, dvector, xx = xx,
    bw = bw)
# (6)
  ccr <- solve(cc)
  almat <- ccr %*% dmat
# (7)
  mm <- as.vector(beta1 %*% t(gg))
  exmat <- t(apply(matrix(c(0:(np - 1)), nrow = 1), 2,
    powerf, x1 = ex))
  ey <- beta1 %*% exmat + as.vector(t(almat) %*% (yy - mm))
# (8)
  return(ey)
}

## An example of an object to use mykrig2()
## Construction of figure 4.8
# (1)
nd <- 20
set.seed(123)
xx <- seq(from = 1 , by = 1, length = nd)
yy <- sin(0.1 * pi * xx) + 1 + 
  rnorm(nd, mean =0, sd = 0.5)
# (2)
ne <- 96
ex <- seq(from = 1, by = 0.2, length = ne)
band <- c(0.1, 0.3, 1, 3)
np = 4 # cubic equation
# (3)
par(mfrow = c(2, 2))
# (4)
for(jj in 1:4) { 
  # (5)
  bw <- band[jj]
  ey <- mykrig2(nd, ne, xx, yy, ex, bw, np)
  # (6)
  plot(xx, yy, type = "n", xlab = "x",
       ylab = "y", ylim = c(-1, 3), main = paste0("delta : ", bw))
  points(xx, yy, cex = 1.2, pch =2)
  lines(ex, ey, lwd =2)
} 


##(H) Universal kriging with two predictors 
##A rough trend is represented by a polynomial equation'
##Construction of figure 4.10 
# (1)
library(spatial)
# (2)
xx1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
xx2 <- c(7, 5, 4, 2, 9, 5, 3, 4, 2, 9)
yy<- c(2, 7, 4, 3, 5, 2, 9, 4, 1, 8)
# (3)
par(mfrow = c(2, 2), mai = c(0.3, 0.3, 0.2, 0.2),
  oma = c(0.5, 0.5, 0.5, 0.5))
# (4)
fit.gls <- surf.gls(np = 2, gaucov, x = xx1, y = xx2, 
  z = yy, d = 0.2)
# (5)
krig <- prmat(fit.gls, 1, 10, 1, 10, 19)
# (6)  
persp(krig, zlim = c(0, 12), xlab = "x1", ylab = "x2",
  zlab = "y", lab = c(3, 3, 3), main = "delta : 0.2")
# (7)
fit.gls <- surf.gls(np = 2, gaucov, x = xx1, y = xx2, 
  z = yy, d = 0.5)
krig <- prmat(fit.gls, 1, 10, 1, 10, 19)
persp(krig, zlim = c(0, 12), xlab = "x1", ylab = "x2",
  zlab = "y", lab = c(3, 3, 3), main = "delta : 0.5")
fit.gls <- surf.gls(np = 2, gaucov, x = xx1, y = xx2, 
  z = yy, d = 1)
krig <- prmat(fit.gls, 1, 10, 1, 10, 19)
persp(krig, zlim = c(0, 12), xlab = "x1", ylab = "x2",
  zlab = "y", lab = c(3, 3, 3), main = "delta : 1")
fit.gls <- surf.gls(np = 2, gaucov, x = xx1, y = xx2, 
  z = yy, d = 2)
krig <- prmat(fit.gls, 1, 10, 1, 10, 19)
persp(krig, zlim = c(0, 12), xlab = "x1", ylab = "x2",
  zlab = "y", lab = c(3, 3, 3), main = "delta : 2")
  

##(I) Universal kriging with two predictors
##A rough trend is represented by a polynomial equation
##Construction of figure 4.12
# (1)
library(spatial)
# (2)
dd <- 0.8
# (3)
nd <- 100
set.seed(523)
xx1 <- runif(nd, min = 0, max = 10)
xx2 <- runif(nd, min = 0, max = 10)
yy <- 8 +5 * sin(xx1^2/10) + 3 * cos(xx2 * 2) +
  3 * sin(xx2 * 2.5) + xx2 * 0.6
# (4)
par(mfrow = c(2, 2))
# (5)
fit.gls <- surf.gls(1, gaucov, x = xx1, y = xx2,
  z = yy, d = dd)
# (6)
cor1 <- correlogram(fit.gls, nint = 40, plotit = F)
plot(cor1$x, cor1$y, type = "n", xlab = "distance",
  ylab = "cor")
points(cor1$x, cor1$y, pch = 0, cex = 0.8)
# (7)
rr <- seq(from =0, to = 12, by = 0.2)
lines(rr, gaucov(r = rr, d = dd), lwd =3)
# (8)
var1 <- variogram(fit.gls, nint = 40, plotit = F)
plot(var1$x, var1$y, type = "n", xlab = "distance",
  ylab = "var")
points(var1$x, var1$y, pch =5, cex =0.8)
# (9)
fit.gls <- surf.gls(1, gaucov, x = xx1, y = xx2,
  z = yy, d = dd)
krig <- prmat(fit.gls, 1, 10, 1, 10, 30)
persp(krig, zlim = c(0, 20), xlab = "x1", ylab = "x2",
  zlab = "y", lab = c(3, 3, 3), phi = 20, theta = 30)



##(J) Derivation of an additive model by solving 
##a normal equation by Gauss-Seidel method
myadd1<-function(nd, it, xx1, xx2, yy, deg1, deg2)
{
# (1)
  diag1 <- diag(nd)
# (2)
  polyreg <- function(yy, xx, degree)
  {
    data1 <- data.frame(x1 =  xx, y1 = yy)
    d1 <- degree 
    formula.name <- substitute(y1 ~ poly(x1, degree = d2),
      list(d2 = d1))
    fit.lm <- lm(formula.name, data = data1)
    return(fit.lm$fitted.values)
  }
  hh1 <- apply(diag1, 2, polyreg, xx = xx1, degree = deg1)
# (3)
  hh2 <- apply(diag1, 2, polyreg, xx = xx2, degree = deg2)
# (4)
  av1 <- 0
  mm2 <- rep(av1, length = nd)
  for(ii in 1 :it) {
    mm1 <- hh1 %*% (yy - mm2)
    mm2 <- hh2 %*% (yy - mm1)
  }
# (5)
  return(list(mm1 = mm1, mm2 = mm2))
}
 
#For the sum of the elements of mm1 to be 0
#hh1 <- hh1 - matrix(rep(1, length = nd * nd), ncol = nd)/nd

#replace (2) for B-spline case instead of polynomial equation
  bsreg <- function(yy, xx, knots, degree)
  {
    data1 <- data.frame(x1 = xx, y1 = yy)
    kn1 <- knots
    d1 <- degree
    formula.name <- substitute(y1 ~ bs(x1, knots = kn2,
      degree = d2), list(kn2 = kn1, d2 = d1))
    fit.lm <- lm(formula.name, data = data1)
    return(fit.lm$fitted.values)
  }
  diag1 <- diag(nd)
  hh1 <- apply(diag1, 2, bsreg, xx = xx1,
    knots = c(2, 4, 6, 8), degree = deg1)
# (3)
  hh2 <- apply(diag1, 2, bsreg, xx = xx2,
    knots = c(3, 7), degree = deg2)

  
## An example of myadd1() 
## Construction of figure 4.14
# (1)
nd <- 40
it <- 1
# (2)
set.seed(100)
xx1 <- runif(nd, min =0, max = 10)
xx2 <- runif(nd, min =0, max = 10)
yy <- sin(0.2 * pi * xx1) + xx2^2 * 0.05 - xx2 * 0.4 +
  3 + rnorm(nd, mean = 0, sd = 0.1)
# (3)
deg1 <- 4
deg2 <- 2
fit.add <- myadd1(nd, it, xx1, xx2, yy, deg1, deg2)
mm1 <- fit.add$mm1
mm2 <- fit.add$mm2
# (4)
par(mfrow = c(1, 2), mai = c(0.5, 0.5, 0.3, 0.3),
  oma = c(1, 1, 1, 1))
plot(xx1, mm1, type = "n", xlab = "x1", ylab = "m1(x1)",
  ylim = c(1, 4))
points(xx1, mm1, cex = 1.2, pch =2)
plot(xx2, mm2, type = "n", xlab = "x2", ylab = "m2(x2)",
  ylim = c(-0.5, 1.5))
points(xx2, mm2, cex = 1.2, pch =5)


##(K) Additive model by solving a normal equation 
## by Gauss-Seidel method
library(gam)

sadd1<-function(nd, ne, xx1, xx2, yy, ex1, ex2, ey1)
{
# (1)
  assign("data1", data.frame(x1 = xx1, x2 = xx2, y = yy))
# (2)
  fit.gam <- gam(y ~ s(x1) + s(x2), data = data1)
# (3)
  data2 <- data.frame(x1 = xx1, x2 = xx2)
  fit.tmd <- predict(fit.gam, newdata = data2,
    type = "terms")
# (4)
  eyd1 <- fit.tmd[, 1]
  eyd2 <- fit.tmd[, 2] + attr(fit.tmd, "constant")
# (5)
  data3 <- data.frame(x1 = ex1, x2 = ex2)
  fit.tme <- predict(fit.gam, newdata = data3,
    type = "terms")
# (6)
  ey1 <- fit.tme[, 1]
  ey2 <- fit.tme[, 2] + attr(fit.tme, "constant")
# (7)
  return(list(eyd1 = eyd1, eyd2 = eyd2, ey1 = ey1, ey2 = ey2))
}
  

## An example of sadd1()
## Construction of figure 4.20
# (1)
nd <- 40
set.seed(100)
xx1 <- runif(nd, min =0, max =10)
xx2 <- runif(nd, min =0, max =10)
yy <- sin(0.2 * pi * xx1) + xx2^2 * 0.05 - xx2 * 0.4 + 3 + rnorm(nd, mean = 0, sd = 0.1)
# (2)
ne <- 100
ex1 <- seq(from = min(xx1), to = max(xx1), length = ne)
ex2 <- seq(from = min(xx2), to = max(xx2), length = ne)
# (3)
fit.add <- sadd1(nd, ne, xx1, xx2, yy, ex1, ex2, ey1)
eyd1 <- fit.add$eyd1
eyd2 <- fit.add$eyd2
ey1 <- fit.add$ey1
ey2 <- fit.add$ey2
# (4)
par(mfrow = c(1, 2))
plot(ex1, ey1, type = "n", xlab = "x1", ylab = "m1(x1)")
lines(ex1, ey1, lwd =2)
points(xx1, eyd1, pch =2)
plot(ex2, ey2, type = "n", xlab = "x2", ylab = "m2(x2)")
lines(ex2, ey2, lwd =2)
points(xx2, eyd2, pch =2)


##(L) Derivation of regression equation with the form of ACE 
acecan1<-function(nd, npx1, npx2, npy, xx1, xx2, yy)
{
# (1)
  powerf <- function(jj, x1)
  {
    pw <- x1^jj
    return(pw)
  }
  xxm1 <- apply(matrix(c(1:npx1), nrow = 1), 2,
    powerf, x1 = xx1)
  xxm2 <- apply(matrix(c(1:npx2), nrow = 1), 2,
    powerf, x1 = xx2)
  xxmat <- cbind(xxm1, xxm2)
  yymat <- apply(matrix(c(1:npy), nrow = 1), 2,
    powerf, x1 = yy)
# (2)
  can1 <- cancor(xxmat, yymat, xcenter = T, ycenter = T)
  cor1 <- can1$cor[1]
# (3)
  ex1 <- sqrt(nd) * can1$xcoef [1 :npx1, 1] %*%
    sweep(t(xxm1), 1, can1$xcenter[1:npx1])
  ex2 <- sqrt(nd) * can1$xcoef[(npx1 + 1):(npx1 + npx2),
    1] %*% sweep(t(xxm2), 1, can1$xcenter [1 :npx2] )
  ey <- sqrt(nd) * can1$ycoef[1 :npy, 1] %*%
    sweep(t(yymat), 1, can1$ycenter[1:npy])
  ex1 <- ex1 * cor1
  ex2 <- ex2 * cor1
  ex12 <- ex1 + ex2
# (4)
  return(list(ex1= ex1, ex2 = ex2, ex12 = ex12, ey = ey, cor1 = cor1))
}
 

## An example of acecan1()
## Construction of figure 4.24

# (1)
nd <- 100
set.seed(100)
xx1 <- runif(nd, min =0, max = 10)
xx2 <- runif(nd, min =0, max = 10)
yy <- (sin(0.2 * pi * xx1) + xx2^2 * 0.05 - xx2 * 0.4
  + 3 + rnorm(nd, mean =0, sd = 0.1))^2
# (2)
npx1 <- 4
npx2 <- 2
npy <- 2
# (3)
fit.ace <- acecan1(nd, npx1, npx2, npy, xx1, xx2, yy)
print(fit.ace$cor1)
ex1 <- fit.ace$ex1
ex2 <- fit.ace$ex2
ex12 <- fit.ace$ex12
ey <- fit.ace$ey
# (4)
par(mfrow = c(2, 2), mai = c(1, 0.5, 0.5, 0.5),
  oma = c(1, 1, 1, 1))
# (5)
plot(xx1, ex1, type = "n", xlab = "x1", ylab = "m1(x1)")
points(xx1, ex1, cex = 0.7, pch =0)
# (6)
plot(xx2, ex2, type = "n", xlab = "x2", ylab = "m2(x2)")
points(xx2, ex2, cex = 0.7, pch =2)
# (7)
plot(yy, ey, type = "n", xlab = "y", ylab = "eta(y)")
points(yy, ey, cex = 0.7, pch =5)
# (8)
plot(ex12, ey, type = "n", xlab = "m1(x1)+m2(x2)",
  ylab = "eta(y)", xlim = c(-1.7, 2.5), ylim = c(-1.7, 2.5))
points(ex12, ey, cex = 0.7, pch = 6)


##(M) Derivation of a regression equation with the form 
## of ACE by ACE algorithm
aceit1<-function(nd, it, npx1, npx2, npy, xx1, xx2, yy)
{ 
# (1)
  yyst <- yy
# (2)
  powerf <- function(jj, x1)
  {  
    pw <- x1^jj
    return(pw)
  }
  xxm1 <- apply(matrix(c(1:npx1), nrow = 1),
    2, powerf, x1 = xx1)
  xxm2 <- apply(matrix(c(1:npx2), nrow = 1),
    2, powerf, x1 = xx2)
  xxmat <- cbind(xxm1, xxm2)
  xxmean <- apply(xxmat, 2, mean)
  xxmat <- sweep(xxmat, 2, xxmean)
  yymat <- apply(matrix(c(1:npy), nrow = 1),
    2, powerf, x1 = yy)
  yymean <- apply(yymat, 2, mean)
  yymat <- sweep(yymat, 2, yymean)
  hatx <- xxmat %*% solve (crossprod (xxmat)) %*% t(xxmat)
  haty <- yymat %*% solve(crossprod(yymat)) %*% t(yymat)
  hatyx <- haty %*% hatx
# (3)
  for(ii in 1 :it) { 
    yyst <- hatyx %*% yyst
    yyst <- (sqrt(nd) * yyst)/sqrt(sum(yyst^2))
  }
# (4)
  return(yyst)
}

aceit2<-function(nd, it, npx1, npx2, npy, xx1, xx2, yy)
{
  # (1)
  yyst <- (yy - mean(yy))/sqrt(sum((yy - mean(yy))^2)/nd)
  # (2)
  for(ii in 1:it) { 
    # (3)
    data1 <- data.frame(x1 = xx1, x2 = xx2, y = yyst)
    formula1 <- substitute(y ~ poly(x1, degree = deg1) +
                             poly(x2, degree = deg2), list(deg1 = npx1, deg2 = npx2))
    fit.lm <- lm(formula1, data = data1)
    yhat <- fitted.values(fit.lm)
    # (4)
    data2 <- data.frame(x = yy, y = yhat)
    formula2 <- substitute(y ~ poly(x, degree = deg3),
                           list(deg3 = npy))
    fit.lm <- lm(formula2, data = data2)
    eta <- fitted.values(fit.lm)
    # (5)
    yyst <- eta/sqrt(sum((eta - mean(eta))^2)/nd)
    print(sqrt(sum((yyst - mean(yyst))^2)/nd))
  }
  # (6)
  return(yyst)
}
   

## An example of aceit1()
## Construction of figure 4.27
# (1)
nd <- 100
set.seed(100)
xx1 <- runif(nd, min =0, max = 10)
xx2 <- runif(nd, min =0, max = 10)
yy <- (sin(0.2 * pi * xx1) + xx2^2 * 0.05 - xx2 * 0.4
  + 3 + rnorm(nd, mean =0, sd = 0.1))^2 
# (2)         
npx1 <- 4
npx2 <- 2
npy <- 2
# (3)
par(mfrow = c(2, 2))
# (4)
for(it in 1:4) {
  yyst <- aceit1(nd, it, npx1, npx2, npy, xx1, xx2, yy)
  plot(yy, yyst, type = "n", xlab = "y", ylab = "y*", main = paste("Iteration", it))
  points(yy, yyst, cex = 0.7, pch =5)
  }

# add the object eyd1, eyd2
assign("data1", data.frame(x1 = xx1, x2 = xx2, y = yyst))
formula1 <- substitute(y ~ poly(x1, degree = deg1)
  + poly(x2, degree = deg2), list(deg1 = npx1, deg2 = npx2))
fit.lm <- lm(formula1, data = data1)
assign("data2", data.frame(x1 = xx1, x2 = xx2))
fit.tmd <- predict(fit.lm, newdata = data2,
  type = "terms")
eyd1 <- fit.tmd[, 1]
eyd2 <- fit.tmd[, 2] + attr(fit.tmd, "constant")
yhat <- eyd1 + eyd2
plot(xx1, eyd1, type = "n", xlab = "x1", ylab = "m1(x1)")
points(xx1, eyd1, cex = 1.2, pch =5)
plot(xx2, eyd2, type = "n", xlab = "x2", ylab = "m2(x2)")
points(xx2, eyd2, cex = 1.2, pch =5)

  
  
##(O) Derivation of regression equation by the use 
## of ACE based on super-smoother 
# (1)
library(acepack)
nd <- 100
set.seed(100)
xx1 <- runif(nd, min = 0, max =10) 
xx2 <- runif(nd, min = 0, max = 10)
yy <- (sin(0.2 * pi * xx1) + xx2^2 * 0.05 - xx2 * 0.4
  + 3 + rnorm(nd, mean = 0, sd = 0.1))^2
# (2)
xxmat <- cbind(xx1, xx2)
fit.ace <- ace(xxmat, yy)
eys1 <- fit.ace$tx[, 1] 
eys2 <- fit.ace$tx[, 2]
yy2 <- fit.ace$ty
# (3)
par(mfrow = c(2, 2))
plot(xx1, eys1, type = "n", xlab = "x1", ylab = 
  "m1(x1)")
points(xx1, eys1, cex =0.7, pch =0)
plot(xx2, eys2, type = "n", xlab = "x2", ylab =
  "m2(x2)")
points(xx2, eys2, cex =0.7, pch =2)
plot(yy, yy2, type = "n", xlab = "y", ylab =
  "eta(y)")
points(yy, yy2, cex = 0.7, pch =5)
plot(eys1 + eys2, yy2, type = "n", xlab =
  "m1(x1)+m2(x2)", ylab = "eta(y)")
points(eys1 + eys2, yy2, cex = 0.7, pch =5)

  
##(P) Derivation of a regression equation 
## by projection pursuit regression
## Construction of figure 4.29
# (1)
nd <- 100
set.seed(100)
xx1 <- runif(nd, min =0, max = 10)
xx2 <- runif(nd, min =0, max = 10)
yy <- 2 + cos(xx1 * 0.6 + xx2 * 0.4) + 
  rnorm(nd, mean = 0, sd = 0.1)
# (2)
xmat <- cbind(xx1, xx2)
fit.pro <- ppr(xmat, yy, nterms = 1, min.term = 1, max.term = 7)
print(fit.pro$gofn)
# (3)
nex1 <- 21
nex2 <- 21
ex1 <- seq(from = 0, by = 0.5, length = nex1)
ex2 <- seq(from = 0, by = 0.5, length = nex2)
grid <- expand.grid(ex1, ex2)
grid12 <- cbind(grid[, 1], grid[, 2])
# (4)
fit.pro <- ppr(xmat, yy, nterms =1, min.term = 1, max.term = 7,
  xpred = grid12)
# (5)
ff1x <- xx1*fit.pro$alpha[1] + xx2 * fit.pro$alpha[2]
ff1y <- fit.pro$fitted.values
# (6)
exy <- NULL
exy$x <- ex1
exy$y <- ex2
exy$z <- matrix(predict(fit.pro, grid12), ncol = nex2)
# (7)
par(mfrow = c(1, 2))
plot(ff1x, ff1y, type = "n", xlab = "z", ylab = "m1(z)")
points(ff1x, ff1y, pch = 15)
# (8)
persp(x = exy$x, y = exy$y, z = exy$z, xlab = "x1", ylab = "x2", zlab = "y",
  r = 10, theta = -30, phi = 30, lab = c(3, 3, 1))
