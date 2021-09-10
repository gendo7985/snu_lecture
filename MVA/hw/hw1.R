# Libraries ---

library(ggplot2)
library(ggrepel)
library(GGally)
library(dplyr)

# Exercises ---------------------------------------------------------------

# 1. Using the datasets `USArrests` and `state`, 
# create a data frame containing all 50 states of USA 
# measured for area (size), region, and four types of crime rates. 
# Resulting dataset must have 5 numeric variables and one categorical variable.

data(USArrests)
data(state)

USArrests.m <- cbind(state.area, state.region, USArrests)

# 2. Draw a scatterplot matrix using all numeric variables, 
# colored by different regions.
scatplot <- ggpairs(USArrests.m,
                    columns = c(1, 3:6),
                    aes(color = state.region))
scatplot
# 3. Compute the sample mean for each region. 

sample_mean <- USArrests.m %>%
  group_by(state.region) %>%
  summarise(area = mean(state.area),
            Murder = mean(Murder),
            Assault = mean(Assault),
            UrbanPop = mean(UrbanPop),
            Rape = mean(Rape))
sample_mean

# 4. Overlay the five sample means on the scatterplot matrix. 
# Add appropriate labels.
grand_mean <- sapply(USArrests.m[-2], mean)
name <- names(grand_mean)
for (j in 1:5){
  for (i in j + seq_len(5 - j)){
    scatplot[i, j] <- scatplot[i, j] +
      geom_point(x = grand_mean[j], y = grand_mean[i], color = "red", shape = 4) +
      annotate("text",
               x = grand_mean[j], y = grand_mean[i],
               label = paste0("(",name[j],", ",name[i],")"),
               color = "darkred", size = 4, fontface = "italic", family = "serif",
               vjust = -1, hjust = 0.2)
  }
}
scatplot

# 5. Ignore the categorical variable. For each observation, 
# compute the mahalanobis distance from the (grand) sample mean
# and use it to sort from the closest to furthest from the mean. 
# Report the ordering.

USArrests.m <- USArrests.m[-2]
ordered_USArrests <- USArrests.m  %>%
  mutate(m_dist = mahalanobis(USArrests.m, grand_mean, cov(USArrests.m))) %>%
  arrange(m_dist)
# ordered_USArrests
head(ordered_USArrests)

# 6. Consider the plane containing the mean (computed above) and 
# the two furthest states from the mean. Create orthonormal basis 
# spanning the plane, and display the projections of all observations 
# on the plane. Be sure to label the points with their names (or abbr's.)

# pass

# 7. Now use the eigenvectors (cf: eigen(cov(X))$vectors) to change 
# the coordinates. Compute the sample covariance matrix in the new coordinate system.

new.USArrests <- as.matrix(USArrests.m) %*% eigen(cov(USArrests.m))$vectors
cov(new.USArrests)
