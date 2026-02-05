library(ggplot2)
library(dplyr)

df <- read.csv("https://raw.githubusercontent.com/roualdes/data/refs/heads/master/penguins.csv")

ggplot(data = df, aes(body_mass_g, flipper_length_mm)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

fit <- lm(flipper_length_mm ~ body_mass_g, data = df)
summary(fit)

confint(fit, level = 0.98)

predict(fit)
predict(fit, newdata = data.frame(body_mass_g = c(4000, 4001)), 
        interval = "confidence", level = 0.95)

diff(predict(fit, newdata = data.frame(body_mass_g = c(4000, 4001))))

x <- fit$model$body_mass_g
predict(fit, newdata = data.frame(body_mass_g = c(3000, mean(x))), 
        interval = "confidence", level = 0.95)

# R-squared
y <- fit$model$flipper_length_mm
yhat <- predict(fit)
SSF <- sum((y - yhat) ^ 2)
SSR <- sum((y - mean(y)) ^ 2)
1 - SSF / SSR

# adjusted R-squared
N <- length(y)
K <- length(coef(fit))
1 - (SSF / (N - K)) / (SSR / (N - 1))
