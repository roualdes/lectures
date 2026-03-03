library(dplyr)
library(ggplot2)

fit <- lm(y ~ x, data = df)

r <- rstandard(fit)
dfr <- data.frame(r = r)

ggplot(data = dfr) + geom_histogram(aes(r))

dfr$yhat <- predict(fit)

ggplot(data = dfr) + geom_point(aes(yhat, r))


