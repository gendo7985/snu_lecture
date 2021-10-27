# Multivariate analysis lab 
# Written by S. Jung
# Version date: June 2, 2019

## Principal Component Analysis


# 3D scatterplots ---------------------------------------------------------
# PCA is used for dimension reduction, but also for exploration. 
# Scatterplot matrix is extremely useful for this purpose, 
# but there are times where 3D scatterplots are useful. 
# Here're some options in R. 

# Interactive 3D scatterplot (plot.ly)
library(plotly)
 
mtcars$am[which(mtcars$am == 0)] <- 'Automatic'
mtcars$am[which(mtcars$am == 1)] <- 'Manual'
mtcars$am <- as.factor(mtcars$am)

p <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec, color = ~am, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Weight'),
                      yaxis = list(title = 'Gross horsepower'),
                      zaxis = list(title = '1/4 mile time')))
p # You may or may not see the result depending on your HTML browser
# Learn more at https://plot.ly/r/getting-started/

# Interactive 3D scatterplot (rgl + car)
library(rgl)
plot3d(mtcars[,c("wt","hp","qsec")])

library(car)
scatter3d(wt ~ hp + qsec, data = mtcars)
# See `?scatter3d` for more options

# Static 3D scatterplot (scatterplot3d)
library(scatterplot3d)
scatterplot3d(mtcars[,c("wt","hp","qsec")])

# PCA: Practice -----------------------------------------------------------
# The standard data format is the n × p data frame or matrix x.

x <- mtcars[,sapply(mtcars, is.numeric)] 

# To perform PCA by eigen decomposition:
spr <-princomp(x)
summary(spr)
U <- spr$loadings
L <- (spr$sdev)^2
Z <- spr$scores


# To perform PCA by singular value decomposition
gpr <- prcomp(x)
summary(gpr)
U <- gpr$rotation
L <- (gpr$sdev)^2
Z <- gpr$x

# Exercises ---------------------------------------------------------------
# 1. Perform Correlation PCA using the standardized dataset. 
#     (Hint: use the argument "scale" or "scale.")

# 2. Compare the scatters of the two first TWO PC scores from 
#     (a) the usual PCA and (b) the correlation PCA

# 3. Compare the Principal Component variances from (a) and (b)

# 4. Which choice is deemed more appropriate here? 

# PCA: Biplot -------------------------------------------------------------
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot) 
mtcars.pc  <- prcomp(x, scale. = TRUE)
ggbiplot(mtcars.pc, labels=rownames(mtcars))

groups <- mtcars[,!sapply(mtcars, is.numeric)] 
ggbiplot(mtcars.pc, labels=rownames(mtcars), groups = groups)
# Can you interpret PC1, PC2 directions w.r.t their relation with variables and samples? 

ggbiplot(mtcars.pc, labels=rownames(mtcars), groups = groups, ellipse=TRUE)
ggbiplot(mtcars.pc, choices=c(3,4), labels=rownames(mtcars), groups = groups, ellipse=TRUE)


# Exercises ---------------------------------------------------------------
# 5. Take the "USArrests" data set, and perform a PCA. Create a biplot and interpret PC1 and PC2. 
# 6. What percentages of the variation is explained by the first two PC? 

