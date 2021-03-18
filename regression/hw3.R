library(ggplot2)
# Q.1

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color=Species)) +
  theme_bw()


# Q.2

model = lm(Sepal.Length ~ Petal.Length, data = iris)
model$coefficients[2]
