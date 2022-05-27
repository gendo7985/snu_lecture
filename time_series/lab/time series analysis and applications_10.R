# rm(list=ls())

# install.packages("fGarch")
# install.packages("rugarch")
library(fGarch)
library(rugarch)

#### fGarch package
### sampling
?garchSpec
?garchSimr

## ARCH(1)
spec_arch <- garchSpec(model = list(omega=1, alpha=0.3, beta=0))
spec_arch
data_arch <- garchSim(spec = spec_arch, n=1000) # take saple from ARCH(1)
plot(data_arch)

## GARCH(1,1)
spec_garch <- garchSpec(model = list(omega=1, alpha=0.3, beta=0.4), rseed=1) # rseed = set.seed
data_garch <- garchSim(spec = spec_garch, n=1000)
plot(data_garch)

## GARCH(1,1) ;epsilon_t ~ t(4)
spec_garch_t <- garchSpec(model = list(omega=1, alpha=0.3, beta=0.4, shape = 4),
                         rseed=1, cond.dist="std") # "norm", "ged", "std" // "snorm", "sged", "sstd"
data_garch_t <- garchSim(spec = spec_garch_t, n=1000)
plot(data_garch_t)

# extended
data_garch2 <- garchSim(spec = spec_garch, n = 1000, extended=T)
plot(data_garch2)

data_garch_t2 <- garchSim(spec = spec_garch_t, n = 1000, extended=T)
plot(data_garch_t2)

hist(data_garch2$eps, breaks = seq(-8, 8, length.out = 50))
hist(data_garch_t2$eps, breaks = seq(-8, 8, length.out = 50))

## AR(1)-GARCH(1,1)
spec_argarch <- garchSpec(model = list(mu = 0, ar = 0.9, ma=0,
                                       omega=1, alpha=0.3, beta=0.5), rseed=1)
data_argarch <- garchSim(spec_argarch, n = 1000)
plot(data_argarch)


### data fitting

## fitting garch(1,1)
?garchFit
fit_garch <- garchFit(formula = ~ garch(1,1), data = data_garch) # include.mean = TRUE

typeof(fit_garch)
names(fit_garch) ## not working!
slotNames(fit_garch)

fit_garch@fit$par
para=as.list(fit_garch@fit$par)
para$omega/(1-para$alpha1-para$beta1)
var(as.numeric(data_garch))

# fitted value(mean) & residuals
as.numeric(fit_garch@fitted)
fit_garch@fit$coef[1]

residuals(fit_garch) == fit_garch@residuals
sum(as.numeric(fit_garch@residuals - data_garch-fit_garch@fit$coef[1])^2)

# standardized residuals(xi_t)
residuals(fit_garch, standardize=T) == fit_garch@residuals/fit_garch@sigma.t

mean(residuals(fit_garch, standardize = T))
var(residuals(fit_garch, standardize = T))

# fitting garch(1,1) with std
fit_garch2 <- garchFit(formula = ~ garch(1,1), data = data_garch_t$garch) # normal
fit_garch2@fit$par
fit_garch2@fit$ics
plot(residuals(fit_garch2, standardize=T), type='l')

fit_garch_t <- garchFit(formula = ~ garch(1,1), data = data_garch_t$garch, 
                         cond.dist="std", include.shape = TRUE) # std
fit_garch_t@fit$par
fit_garch_t@fit$ics
plot(residuals(fit_garch_t, standardize=T), type='l')

hist(residuals(fit_garch2, standardize=T), breaks = seq(-7, 7, length.out = 50))
hist(residuals(fit_garch_t, standardize=T), breaks = seq(-7, 7, length.out = 50))

### residual analysis
library(tseries)
# normality
jarque.bera.test(residuals(fit_garch2, standardize=T))
shapiro.test(residuals(fit_garch2, standardize=T))
ks.test(residuals(fit_garch2, standardize=T), "pnorm", 0, 1)

# e_t~t(4)
ks.test(residuals(fit_garch2, standardize=F), "pt", 4)

### forecast
predict(fit_garch_t,  n.ahead = 10, plot=T)

fit_argarch = garchFit(formula = ~ arma(1,0)+garch(1,1), data = data_argarch$garch)
predict(fit_argarch,  n.ahead = 10, plot=T)



#### rugarch package
spec_armagarch <- garchSpec(model = list(mu = 1, ar = 0.4, ma=-0.2,
                                         omega=1, alpha=0.3, beta=0.5), rseed=1)
data_armagarch <- garchSim(spec_armagarch, n = 1000)
plot(data_armagarch)

### fitting arma(1,1)-garch(1,1)
spec = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                  distribution = 'norm')
show(spec)
armagarchfit = ugarchfit(spec, data_armagarch[1:500, , drop = FALSE])
as.list(coef(armagarchfit))

spec2 = getspec(armagarchfit)
setfixed(spec2) <- as.list(coef(armagarchfit))

### forecasting without re-estimation
forc1 = ugarchforecast(armagarchfit, n.ahead = 500)
plot(forc1)
slotNames(forc1)

forc2 = ugarchforecast(spec2, n.ahead = 1, n.roll = 499,
                       data = data_armagarch[1:1000, , drop = FALSE], out.sample = 500)
plot(forc2)
as.numeric(forc2@forecast$seriesFor) # predicted value

### forecasting with re-estimation
# create a cluster object to be used as part of this demonstration
cluster = makePSOCKcluster(4)

spec3 = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                  distribution = 'norm')
roll = ugarchroll(spec3, data_armagarch, forecast.length = 500, refit.every = 10, 
                  refit.window = 'moving', window.size = 500, calculate.VaR = FALSE, 
                  keep.coef = TRUE, cluster = cluster)
show(roll)
plot(roll)
roll@forecast$density$Mu # predicted value

## RMSE
sqrt(mean(c(data_armagarch[501:1000]-forc1@forecast$seriesFor)^2))
sqrt(mean(c(data_armagarch[501:1000]-forc2@forecast$seriesFor)^2))
sqrt(mean(c(data_armagarch[501:1000]-roll@forecast$density$Mu)^2))

stopCluster(cluster)
