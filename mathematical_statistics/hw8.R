library(ggplot2)
library(dplyr)
library(tidyr)

x = seq(0,10, by = 0.01)
gammas = data.frame(x = x)
gammas = gammas %>%
  mutate("0.1" = dgamma(x, shape = 0.1, scale = 1),
         "0.5" = dgamma(x, shape = 0.5, scale = 1),
         "1" = dgamma(x, shape = 1, scale = 1),
         "2" = dgamma(x, shape = 2, scale = 1),
         "5" = dgamma(x, shape = 5, scale = 1)) %>%
  gather(key = "shape", value = "y", -x)
p <- ggplot(data = gammas, aes(x = x, y = y, group = shape, color = shape))+
  geom_line()+theme_bw()+
  labs(title="X ~ Gamma(shape, 1)")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  coord_cartesian(xlim = c(0,10), ylim = c(0,1.5))
p

x = seq(0,10, by = 0.01)
gammas = data.frame(x = x)
gammas = gammas %>%
  mutate("0.1" = dgamma(x, shape = 2, scale = 0.1),
         "0.5" = dgamma(x, shape = 2, scale = 0.5),
         "1" = dgamma(x, shape = 2, scale = 1),
         "2" = dgamma(x, shape = 2, scale = 2),
         "5" = dgamma(x, shape = 2, scale = 5)) %>%
  gather(key = "scale", value = "y", -x)
p <- ggplot(data = gammas, aes(x = x, y = y, group = scale, color = scale))+
  geom_line()+theme_bw()+
  labs(title="X ~ Gamma(2, scale)")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  coord_cartesian(xlim = c(0,10), ylim = c(0,1))
p

x = seq(0, 1, by = 0.001)
betas = data.frame(x=x) %>%
  mutate("(1, 1)" = dbeta(x, 1, 1),
         "(0.5, 0.5)" = dbeta(x, 0.5, 0.5),
         "(1.5, 1.5)" = dbeta(x, 1.5, 1.5),
         "(0.5, 1.5)" = dbeta(x, 0.5, 1.5),
         "(3, 1)" = dbeta(x, 3, 1),
         "(1, 5)" = dbeta(x, 1, 5))%>%
  gather(key = "parameter", value = "y", -x)
p <- ggplot(data = betas, aes(x = x, y = y, group = parameter, color = parameter))+
  geom_line()+theme_bw()+
  labs(title="X ~ Beta(alpha, beta)",
       color = "(alpha, beta)")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  coord_cartesian(xlim = c(0,1), ylim = c(0,3))
p
