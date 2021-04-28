#############
#           #
#   LAB 8   #
#           #
#############

# setting & libraries

set.seed(0)
library(nls2)
library(dplyr)
library(ggplot2)

#--------------------------------------------

# Example 1: nls

y <- c(44,36,31,39,38,26,37,33,34,48,25,22,44,5,9,13,17,15,21,10,16,22,
       13,20,9,15,14,21,23,23,32,29,20,26,31,4,20,25,24,32,23,33,34,23,28,30,10,29,
       40,10,8,12,13,14,56,47,44,37,27,17,32,31,26,23,31,34,37,32,26,37,28,38,35,27,
       34,35,32,27,22,23,13,28,13,22,45,33,46,37,21,28,38,21,18,21,18,24,18,23,22,
       38,40,52,31,38,15,21)

x <- c(26.22,20.45,128.68,117.24,19.61,295.21,31.83,30.36,13.57,60.47,
       205.30,40.21,7.99,1.18,5.40,13.37,4.51,36.61,7.56,10.30,7.29,9.54,6.93,12.60,
       2.43,18.89,15.03,14.49,28.46,36.03,38.52,45.16,58.27,67.13,92.33,1.17,
       29.52,84.38,87.57,109.08,72.28,66.15,142.27,76.41,105.76,73.47,1.71,305.75,
       325.78,3.71,6.48,19.26,3.69,6.27,1689.67,95.23,13.47,8.60,96.00,436.97,
       472.78,441.01,467.24,1169.11,1309.10,1905.16,135.92,438.25,526.68,88.88,31.43,
       21.22,640.88,14.09,28.91,103.38,178.99,120.76,161.15,137.38,158.31,179.36,
       214.36,187.05,140.92,258.42,85.86,47.70,44.09,18.04,127.84,1694.32,34.27,
       75.19,54.39,79.88,63.84,82.24,88.23,202.66,148.93,641.76,20.45,145.31,
       27.52,30.70)

## model
fo <- y ~ Const + B * (x ^ A)

## starting values (expand.grid)
st1 <- expand.grid(Const = seq(-100, 100, len = 4),
                   B = seq(-100, 100, len = 4),
                   A = seq(-1, 1, len = 4))

## brute-force algorithm
mod1 <- nls2(fo, start = st1, algorithm = "brute-force")
summary(mod1)

## Gauss-Newton algorithm
nls2(fo, start = mod1)

## startinv values (data.frame)
st2 <- data.frame(Const = c(-100, 100), B = c(-100, 100), A = c(-1, 1))
mod2 <- nls2(fo, start = st2, algorithm = "brute-force")
summary(mod2)

nls2(fo, start = mod2)

#--------------------------------------------

# 0. Define Function & Directories

getRsq <- function(y, yhat) {
  1 - sum((y - yhat)^2) / sum((y - mean(y))^2)
}

getMSE <- function(y, yhat) {
  sum((y - yhat)^2) / length(y)
}

setwd('/home/zendo/Github/snu_lecture/regression/hw6')

#--------------------------------------------

# 1. Import Data & Processing

## Read Data
df <- read.csv("global_confirmed_cases_210420.csv")

## Data formatting
df$Date <- sapply(df$Date, function(d) {
  vec <- strsplit(as.character(d), split = "\\.") %>% unlist
  vec_ch <- sapply(vec, function(i) {
    ifelse(nchar(i) < 2, paste0("0", i), i)
  })
  paste(vec_ch, collapse = "-")
})

df_sub <- filter(df, CountryCode == "KOR")

## first case
start_date <- df_sub$Date[min(which(df_sub$Cases > 0))]

df_fin <- df_sub %>%
  mutate(Days_after_Start = as.integer(as.Date(Date) - as.Date(start_date))) %>%
  filter(Days_after_Start >= 0)

#--------------------------------------------

# 2. Model Fitting

## NLS Model: Logistic, Bertalanffy, Gompertz

formul_logit = Cases ~ a / (1 + exp( b - c*Days_after_Start))
grid_logit <- data.frame(a = c(0, max(df_fin$Cases)),
                         b = c(0, 100), c = c(0, 1))
rough_fit_logit <- nls2(formul_logit, data = df_fin, start = grid_logit, alg = "brute-force")
summary(rough_fit_logit)
gn_fit_logit <- nls2(formul_logit, data = df_fin, start = rough_fit_logit)
summary(gn_fit_logit)

formul_bert = Cases ~ a * (1 - exp(- b * Days_after_Start))^c
grid_bert = expand.grid(a = max(df_fin$Cases),
                       b = seq(0, 0.1, 0.001),
                       c = seq(1, 10, 1))
rough_fit_bert <- nls2(formul_bert, data = df_fin, start = grid_bert, alg = "brute-force")
summary(rough_fit_bert)
gn_fit_bert <- nls2(formul_bert, data = df_fin, start = rough_fit_bert)

formul_gomp = Cases ~ a * exp(- b * exp(- c * Days_after_Start))
grid_gomp <- expand.grid(a = max(df_fin$Cases),
                         b = seq(1, 10, 1),
                         c = seq(0, 0.1, 0.001))
rough_fit_gomp <- nls2(formul_gomp, data = df_fin, start = grid_gomp, alg = "brute-force")
summary(rough_fit_gomp)
gn_fit_gomp <- nls2(formul_gomp, data = df_fin, start = rough_fit_gomp,
                    control = nls.control(maxiter = 100))
summary(gn_fit_gomp)

nls_logit <- gn_fit_logit
nls_bert <- rough_fit_bert
nls_gomp <- gn_fit_gomp

## Predict the values with fitted model
y_hat_logit <- predict(nls_logit, df_fin)
y_hat_bert <- predict(nls_bert, df_fin)
y_hat_gomp <- predict(nls_gomp, df_fin)

x <- unique(df_fin$Days_after_Start)
predict <- data.frame(x, y_hat_logit, y_hat_bert, y_hat_gomp)

df_predict <- data.frame(x = rep(predict$x, 3),
                         yhat_cases = c(y_hat_logit, y_hat_bert, y_hat_gomp),
                         yhat_diff = c(y_hat_logit - c(0, y_hat_logit[-length(y_hat_logit)]),
                                       y_hat_bert - c(0, y_hat_bert[-length(y_hat_bert)]),
                                       y_hat_gomp - c(0, y_hat_gomp[-length(y_hat_gomp)])),
                         type = rep(c("Logistic model", "Bertalanffy model", "Gompertz model"), each = nrow(predict)))
df_predict$type <- as.factor(df_predict$type)

#--------------------------------------------

# 3. Visualization: Cumulated & Daily

## preparation
t0 <- start_date
model_labels <- c("Logistic model", "Bertalanffy model", "Gompertz model")
models <- list(nls_logit, nls_bert, nls_gomp)

col_list <- c("red", "blue", "pink")
shape_list <- c("Logistic model" = "dashed",
                "Bertalanffy model" = "solid",
                "Gompertz model" = "dotdash")

## Cumulated
p_1 <- ggplot(data = df_fin, aes(x = Days_after_Start, y = log10(Cases))) +
  geom_point(color = "blue", shape = 1, size = 5) + theme_bw() +
  labs(title = "COVID-19 Cases",
       subtitle = "Korea / Cumulated",
       x = paste('Days Since', t0),
       y = "Number of Cases (log10)") +
  geom_line(data = df_predict,
            aes(x = x, y = log10(yhat_cases), color = type, linetype = type), size = 1) +
  scale_color_manual(name = "Model",
                     labels = model_labels,
                     values = col_list) +
  scale_linetype_manual(name = "Model",
                        labels = model_labels,
                        values = shape_list) +
  theme(plot.title = element_text(size = 25, hjust = 0.5, face = "bold", color = "black", vjust = 2),
        plot.subtitle = element_text(size = 16, hjust = 0.5, face = "italic", color = "maroon", vjust = 2),
        axis.text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(hjust = 0),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))
p_1

MSE_logit <- getMSE(df_fin$Cases, predict$y_hat_logit)
MSE_bert <- getMSE(df_fin$Cases, predict$y_hat_bert)
MSE_gomp <- getMSE(df_fin$Cases, predict$y_hat_gomp)
MSE_logit; MSE_bert; MSE_gomp

## Daily
df_predict_ch <- df_predict[-which(df_predict$x == 0),]
p_2 <- ggplot(data = df_fin, aes(x = Days_after_Start, y = Difference)) +
  geom_point(color = "blue", shape = 1, size = 5) + theme_bw() +
  labs(title = "COVID-19 Cases",
       subtitle = "Korea / Daily",
       x = paste('Days Since', t0),
       y = "Number of Cases (log10)") +
  geom_line(data = df_predict_ch,
            aes(x = x, y = yhat_diff, color = type, linetype = type), size = 1.5) +
  scale_color_manual(name = "Model",
                     labels = model_labels,
                     values = col_list) +
  scale_linetype_manual(name = "Model", labels = model_labels, values = shape_list) +
  theme(plot.title = element_text(size = 25, hjust = 0.5, face = "bold", color = "black", vjust = 2),
        plot.subtitle = element_text(size = 16, hjust = 0.5, face = "italic", color = "maroon", vjust = 2),
        axis.text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(hjust = 0),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))
p_2

MSE_logit_2 <- getMSE(df_fin$Difference[-1], filter(df_predict_ch,type == "Logistic model")$yhat_diff)
MSE_bert_2 <- getMSE(df_fin$Difference[-1], filter(df_predict_ch,type == "Bertalanffy model")$yhat_diff)
MSE_gomp_2 <- getMSE(df_fin$Difference[-1], filter(df_predict_ch,type == "Gompertz model")$yhat_diff)

MSE_logit_2; MSE_bert_2; MSE_gomp_2
