library(ggplot2)
library(dplyr)

df <- read.csv("https://raw.githubusercontent.com/roualdes/data/refs/heads/master/carnivora.csv") %>%
  select(GL, LY, SuperFamily) %>%
  na.omit()

df$sf <- df$SuperFamily == "Caniformia"


fit <- glm(sf ~ GL, data = df, family = "binomial") # logistic regression
summary(fit)

# type = "response" for probabilities
predict(fit, newdata = data.frame(GL = c(104, 105)), type = "response")

# likelihood
ll_logistic <- function(theta, data) {
  X <- data$X
  y <- data$y
  eta <- X %*% theta
  -sum(y * eta - log1p(exp(eta)))
}

# gradient of likelihood
# necessary because this function is harder to minimize
ll_logistic_grad <- function(theta, data) {
  X <- data$X
  y <- data$y
  eta <- X %*% theta
  -t(X) %*% (y - plogis(eta))
}

data <- list(X = model.matrix(fit), y = fit$model$sf)
optim(rep(0, 2), 
      fn = ll_logistic, gr = ll_logistic_grad, 
      data = data, method = "L-BFGS-B")
coef(fit)
