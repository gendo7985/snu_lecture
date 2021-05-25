# 2. F distribution
library(ggplot2)
library(dplyr)
library(tidyr)

x = seq(0,2, by = 0.01)
fs = data.frame(x = x)
fs = fs %>%
  mutate("(5, 5)" = df(x, 5, 5),
         "(20, 5)" = df(x, 20, 5),
         "(5, 20)" = df(x, 5, 20),
         "(20, 20)" = df(x, 20, 20)) %>%
  gather(key = "shape", value = "y", -x)
p <- ggplot(data = fs, aes(x = x, y = y, group = shape, color = shape)) +
  geom_line()+theme_bw()+
  labs(title="X ~ F(df1, df2)")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold")) +
  coord_cartesian(xlim = c(0,2), ylim = c(0,1.5))
p

# 3. t-distribution
x = seq(-3, 3, by = 0.01)
fs = data.frame(x = x)
fs = fs %>%
  mutate("1" = dt(x, 1),
         "3" = dt(x, 3),
         "9" = dt(x, 9),
         "N" = dnorm(x)) %>%
  gather(key = "r", value = "y", -x)

p <- ggplot(data = fs, aes(x = x, y = y, color = r, linetype = r)) +
  geom_line()+theme_bw()+
  scale_linetype_manual(values = c(rep("solid", 3), "dashed"))+
  labs(title="X ~ t(r)")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold")) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(0,0.4))
p
set.seed(0)
# 4. sample distribution
m = 2000
n = 100
Ti <- c()
sqrtn <- sqrt(n)
for (i in 1:m) {
  x <- rnorm(n, 5, 100)
  Ti[i] <- sqrtn * (mean(x) - 5) / sd(x)
}
Ti <- data.frame(x = Ti)
p1 <- ggplot(Ti, aes(x = x)) +
  geom_histogram(aes(y=..density..), color="black", fill = "steelblue",  alpha = 0.2)+
  geom_density(aes(color = "Simulated")) +
  stat_function(aes(color = "t(n-1)"), fun = dt, args = list(df = n-1))+
  theme_bw()+
  labs(title="n = 100", x = "T")+
  scale_color_manual("Density", values = c("blue", "red"))+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))
p1  