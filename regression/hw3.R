library(ggplot2)
# Q.1

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color=Species)) +
  geom_point() +
  theme_bw()


# Q.2
#(a)
model = lm(Sepal.Length ~ Petal.Length, data = iris)
summary(model)

#(b)
my_lm <- function(x, y) {
  mu = mean(x)
  beta_1 = sum((x - mu) * y) / sum((x - mu)^2)
  beta_0 = mean(y - beta_1 * mu)
  coefs = c(beta_0, beta_1)
  names(coefs) = c('beta_0', 'beta_1')
  return(coefs)
}

my_lm(iris$Petal.Length, iris$Sepal.Length)
