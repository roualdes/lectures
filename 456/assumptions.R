library(ggplot2)
library(dplyr)

df <- read.csv("https://raw.githubusercontent.com/roualdes/data/refs/heads/master/finches.csv")

m <- mean(df$beakwidth)
s <- sd(df$beakwidth)

ggplot(data = df) + 
  geom_histogram(aes(x = beakwidth,
                     y = after_stat(density)), bins = 11) +
  geom_function(fun = \(x) dnorm(x, m, s))

dfr <- data.frame(
  r = df$beakwidth - m
)

ggplot(data = dfr) +
  geom_histogram(aes(x=r, y=after_stat(density)), bins = 11)

(5 - m) / s

pnorm((5 - m) / s) * 100

# question about different cutoffs for outliers
q1 <- qnorm(0.25)
q3 <- qnorm(0.75)
iqr <- q3 - q1
q1 - 1.5 * iqr
q3 + 1.5 * iqr


# ANOVA
ggplot(data = df) +
  geom_jitter(aes(island, beakwidth), width = 0.2)

fit <- lm(beakwidth ~ island, data = df)
summary(fit)

dfr <- data.frame(
  # r = residuals(fit)
  r = rstandard(fit)
)

ggplot(data = dfr) + 
  geom_histogram(aes(r), bins = 11)

# Simple Linear Regression
ggplot(data = df, aes(taillength, beakwidth)) +
  geom_point() +
  geom_smooth(method = "lm")

fit <- lm(beakwidth ~ taillength, data = df)
summary(fit)

dfr <- data.frame(
  r = rstandard(fit)
)

ggplot(data = dfr) + 
  geom_histogram(aes(r), bins = 11)

df <- df |>
  mutate(
    r = dfr$r,
    outlier = ifelse(abs(r) > 3, "out", "normal")
    )

ggplot(data = df, 
       aes(taillength, beakwidth, color =  outlier)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# State data
data(state)
state <- as.data.frame(state.x77)

fit <- lm(Income ~ Illiteracy, data = state)
summary(fit)

dfr <- data.frame(
  r = rstandard(fit)
)

ggplot(data = dfr) +
  geom_histogram(aes(r), bins = 11)

state <- state |>
  mutate(
    r = dfr$r,
    yhat = predict(fit)
  )

ggplot(data = state, aes(Illiteracy, Income)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(data = state) +
  geom_point(aes(yhat, r))
