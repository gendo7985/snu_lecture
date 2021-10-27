x <- mtcars[,sapply(mtcars, is.numeric)] 

# To perform PCA by singular value decomposition
gpr1 <- prcomp(x)
summary(gpr1)
U1 <- gpr1$rotation
L1 <- (gpr1$sdev)^2
Z1 <- gpr1$x

# Exercises ---------------------------------------------------------------
# 1. Perform Correlation PCA using the standardized dataset. 
#     (Hint: use the argument "scale" or "scale.")
x_scl <- scale(x, center=TRUE, scale=TRUE)
gpr2 <- prcomp(x_scl)
summary(gpr2)
U2 <- gpr2$rotation
L2 <- (gpr2$sdev)^2
Z2 <- gpr2$x

# 2. Compare the scatters of the two first TWO PC scores from 
#     (a) the usual PCA and (b) the correlation PCA
par(mfrow=c(1,2))
#(a)
plot(Z1[,1], Z1[,2])
title(main="usual PCA")
#(b)
plot(Z2[,1], Z2[,2])
title(main="correlation PCA")
# scale of correlation PCA is within [-4, 4], but usual PCA has large scale

# 3. Compare the Principal Component variances from (a) and (b)
# (a)
summary(gpr1) # 0.927 > 0.07237 > 0.00047 > ...
# (b)
summary(gpr2) # 0.6008 > 0.2409 > 0.05702 > ...
# usual PCA: PC1 is very strong
# correlation PCA: PC1 is relatively weak

# 4. Which choice is deemed more appropriate here? 
# correlation PCA is preffered if measurements are not commensurate.
colnames(mtcars)
# columns of mtcars look commensurate. => usual PCA might be more appropriate.
# PCA: Biplot -------------------------------------------------------------
library(ggbiplot) 

# Exercises ---------------------------------------------------------------
# 5. Take the "USArrests" data set, and perform a PCA. Create a biplot and interpret PC1 and PC2. 
arrests.pc <- prcomp(USArrests, scale. = TRUE)
ggbiplot(arrests.pc, labels=rownames(arrests.pc))
# 6. What percentages of the variation is explained by the first two PC? 
summary(arrests.pc)
# PC1: 62.0%, PC2: 24.7% (from biplot axes) => total 86.7%
