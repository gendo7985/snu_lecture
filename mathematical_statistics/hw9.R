set.seed(0)

n = 100000
X1 <- tan(pi*runif(n)+pi/2)
time = 1:n
X1_t <- cumsum(X1) / time
plot(time, X1_t, type="l")

X2 <- rnorm(n)
X2_t <- cumsum(X2) / time
plot(time, X2_t, type="l")

data <- data.frame(t = time, X1 = X1_t, X2 = X2_t)

library(ggplot2)

p1 <- ggplot(data = data, aes(x = t, y = X1)) +
  geom_line(color = "#00AFBB")+
  theme_bw()
p1

p2 <- ggplot(data = data, aes(x = t, y = X2)) +
  geom_line(color = "#E7B800")+
  theme_bw()
p2
