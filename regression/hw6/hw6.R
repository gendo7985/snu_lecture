library(nls2)
library(dplyr)
library(ggplot2)

# General Functions

getRsq <- function(y, yhat){
  Rsq <- 1 - (sum((y - yhat)^2) / sum((y - mean(y))^2))
  Rsq
}

getMSE <- function(y, yhat){
  MSE <- sum((y - yhat)^2) / length(y)
  MSE
}

# setting
setwd('~/Github/snu_lecture/regression/hw6')

# load data
df <- read.csv("global_confirmed_cases_210420.csv")

# change date "yyyy.m.d" -> "yyyy-mm-dd"
df$Date <- sapply(df$Date, function(d) {
  vec <- as.character(d) %>%
    strsplit(split = ".", fixed = T) %>%
    unlist()
  vec_ch <- sapply(vec, function(i) {
    ifelse(nchar(i) < 2, paste0("0",i), i)
  })
  paste(vec_ch, collapse = "-")
})

df_sub <- df %>% filter(CountryName == "Israel")

start_date <- df_sub$Date[min(which(df_sub$Cases > 0))]

df_fin <- mutate(df_sub, Days_after_Start = as.integer(as.Date(Date) - as.Date(start_date))) %>%
  filter(Days_after_Start >= 0)

# fitting
nls_logit_start <- NULL
nls_bert_start <- NULL
nls_gomp_start <- NULL

formul_logit = Cases ~ a / (1 + exp(b - (c*Days_after_Start)))
grid_logit <- data.frame(a = c(0, max(df_fin$Cases)), b = c(0, 100), c = c(0,1))
rough_fit_logit <- nls2(formul_logit, start = grid_logit, data = df_fin, algorithm = "brute-force")
rough_fit_logit
summary(rough_fit_logit)
coef(rough_fit_logit)
gauss_newton_fit_logit = nls2(formul_logit, data = df_fin, start = rough_fit_logit, trace = T)
gauss_newton_fit_logit
coef(gauss_newton_fit_logit)
summary(gauss_newton_fit_logit)

formul_bert = Cases ~ a * ((1 - exp(-b*Days_after_Start))^c)
grid_bert = data.frame(a = c(0, max(df_fin$Cases)), b = c(0, 0.5), c=c(0, 1000))
rough_fit_bert = nls2(formul_bert, data = df_fin, start= grid_bert, algorithm ="brute-force")
rough_fit_bert
summary(rough_fit_bert)
gauss_newton_fit_bert <- nls2(formul_bert, data=df_fin, start = rough_fit_bert, trace=T)

formul_gomp = Cases ~ a * exp(b * (-1) * exp((-1)*c*Days_after_Start))
grid_gomp = data.frame(a =c(0, max(df_fin$Cases)), b =c(0, 200), c=c(0, 0.3))
rough_fit_gomp = nls2(formul_gomp, data = df_fin, start=grid_gomp, algorithm = "brute-force")
rough_fit_gomp
gauss_newton_fit_gomp <- nls2(formul_gomp, data = df_fin, start=grid_gomp, trace=T)

nls_logit = gauss_newton_fit_logit
nls_bert = rough_fit_bert
nls_gomp = rough_fit_gomp

if(!is.null(nls_logit)){
  y_hat_logit <- predict(nls_logit, df_fin)
}else{
  y_hat_logit <- rep(1, nrow(df_fin))
}

if(!is.null(nls_bert)){
  y_hat_bert <- predict(nls_bert, df_fin)
}else{
  y_hat_bert <- rep(1, nrow(df_fin))
}

if(!is.null(nls_gomp)){
  y_hat_gomp <- predict(nls_gomp, df_fin)
}else{
  y_hat_gomp <- rep(1, nrow(df_fin))
}

x <- unique(df_fin$Days_after_Start)
predict <- data.frame(x,y_hat_logit, y_hat_bert, y_hat_gomp)

df_predict <- data.frame(x=rep(predict$x,3), 
                         yhat_cases = c(y_hat_logit, y_hat_bert, y_hat_gomp),
                         # yhat difference = yhat(t+1) - yhat(t)
                         yhat_difference = c(y_hat_logit - c(0, y_hat_logit[-length(y_hat_logit)]),  
                                             y_hat_bert - c(0, y_hat_bert[-length(y_hat_bert)]),
                                             y_hat_gomp - c(0, y_hat_gomp[-length(y_hat_gomp)])),
                         type = rep(c("Logistic model", "Bertalanffy model", "Gompertz model"), each = nrow(predict)))
df_predict$type <- factor(df_predict$type, levels = c("Logistic model", "Bertalanffy model", "Gompertz model"))


############################# 3. Visualization: Cumulated & Daily #############################

# Visualize preparation
t0 <- as.character(sort(df_fin$Date)[1])
model_labels <- c("Logistic model", "Bertalanffy model", "Gompertz model")
models <- list(nls_logit, nls_bert, nls_gomp)

col_list <- c("red", "blue", "pink")
shape_list <- c("Logistic model"="dashed", "Bertalanffy model"="solid", "Gompertz model"="dotdash")

fitted_idx <- c() # including fitted model only
for(m in 1:length(models)){
  if(!is.null(models[[m]])){
    fitted_idx <- c(fitted_idx, m)
  }
}

# Visualization with ggplot: cumulated
p_1 <- ggplot(data=df_fin,aes(x=Days_after_Start,y=log10(Cases)))+
  geom_point(color='blue', shape = 1, size=5)+theme_bw()+ #중요!
  labs(title = paste0("COVID-19 Cases"), subtitle = paste0("Korea", " / ", "Cumulated"), x=paste0('Days Since ', as.character(t0)),y='Number of Cases (log10)')+
  #scale_y_continuous(breaks=0:5, limits = c(0, 5))+
  geom_line(data=df_predict[which(!is.na(match(df_predict$type, model_labels[fitted_idx]))),],
            aes(x=x,y=log10(yhat_cases), colour = type, linetype = type), size=1.5)+ #중요!
  scale_color_manual(name = "Model",
                     labels = model_labels[fitted_idx],
                     values = col_list[fitted_idx]) +   
  scale_linetype_manual(name = "Model",
                        labels = model_labels[fitted_idx],
                        values = shape_list[fitted_idx]) +
  theme(plot.title=element_text(size=25, hjust=0.5, face="bold", colour="black", vjust=2),
        plot.subtitle=element_text(size=16, hjust=0.5, face="italic", color="maroon", vjust=2),
        axis.text=element_text(size=14, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 14, hjust = 0),
        axis.title=element_text(size=16, colour = "black"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))
p_1
MSE_logit <- getMSE(y = df_fin$Cases, predict$y_hat_logit)
MSE_bert <- getMSE(y = df_fin$Cases, predict$y_hat_bert)
MSE_gomp <- getMSE(y = df_fin$Cases, predict$y_hat_gomp)

#ggsave(p_1, filename = "Confirmed_case_in_KOR_cumulated.png", dpi = 300, width = 12, height = 7)


# Visualization with ggplot: Daily
df_predict_ch <-df_predict[-which(df_predict$x == 0), ]
p_2 <- ggplot(data=df_fin,aes(x=Days_after_Start,y=Difference))+
  geom_point(color='blue', shape = 1, size=5)+theme_bw()+
  labs(title = paste0("COVID-19 Cases"), subtitle = paste0("Korea", " / ", "Daily Cases"), x=paste0('Days Since ', as.character(t0)),y='Number of Cases (log10)')+
  geom_line(data=df_predict_ch[which(!is.na(match(df_predict_ch$type, model_labels[fitted_idx]))),],
            aes(x=x,y=yhat_difference, colour = type, linetype = type), size=1.5)+
  scale_color_manual(name = "Model",
                     labels = model_labels[fitted_idx],
                     values = col_list[fitted_idx]) +   
  scale_linetype_manual(name = "Model",
                        labels = model_labels[fitted_idx],
                        values = shape_list[fitted_idx]) +
  theme(plot.title=element_text(size=25, hjust=0.5, face="bold", colour="black", vjust=2),
        plot.subtitle=element_text(size=16, hjust=0.5, face="italic", color="maroon", vjust=2),
        axis.text=element_text(size=14, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 14, hjust = 0),
        axis.title=element_text(size=16, colour = "black"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))
p_2

#ggsave(p_2, filename = "Confirmed_case_in_KOR_daily.png", dpi = 300, width = 12, height = 7)


MSE_logit_2 <- getMSE(y = df_fin$Difference, y_hat_logit - c(0, y_hat_logit[-length(y_hat_logit)]))
MSE_bert_2 <- getMSE(y = df_fin$Difference, y_hat_bert - c(0, y_hat_bert[-length(y_hat_bert)]))
MSE_gomp_2 <- getMSE(y = df_fin$Difference, y_hat_gomp - c(0, y_hat_gomp[-length(y_hat_gomp)]))

