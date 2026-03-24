library(ggplot2)
library(dplyr)

df <- read.csv("https://raw.githubusercontent.com/roualdes/data/refs/heads/master/carnivora.csv") %>%
  select(GL, LY) %>%
  na.omit()

# normal distribution
ll_normal <- function(theta, data) {
  x <- data$x
  N <- length(x)
  mu <- theta[1]
  sigma <- theta[2]
  d <- x - mu
  N * log(sigma) + 0.5 * sum(d * d) / (sigma * sigma)
}

set.seed(287)
df <- data.frame(x = rnorm(1001, 3.14, 2.71))

optim(rexp(2), ll_normal, data = df, 
      method = "L-BFGS-B", 
      lower = c(-Inf, 1e-5), upper = c(Inf, Inf))


# linear regression
ll_linreg <- function(theta, data) {
  x <- data$x
  y <- data$y
  beta0 <- theta[1]
  beta1 <- theta[2]
  d <- y - (beta0 + beta1 * x)
  sum(d * d)
}

data <- data.frame(x = df$GL, y = df$LY)

optim(rnorm(2), ll_linreg, data = data, method = "L-BFGS-B")
coef(lm(LY ~ GL, data = df))
