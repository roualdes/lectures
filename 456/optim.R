library(dplyr)

df <- read.csv("https://raw.githubusercontent.com/roualdes/data/refs/heads/master/penguins.csv") |>
  select(bill_depth_mm, bill_length_mm) |>
  na.omit()

mean(df$bill_depth_mm)

# mean via optimization
ll_mean <- function(theta, data) {
  y <- data$bill_depth_mm
  return(sum((y - theta) ^ 2))
}

optim(rnorm(1), ll_mean, data=df, method = "L-BFGS-B")


# linear regression line via optimization
ll_lm <- function(theta, data) {
  y <- data$bill_depth_mm
  x <- data$bill_length_mm
  intercept <- theta[1]
  slope <- theta[2]
  return(sum((y - (intercept + slope * x)) ^ 2))
}

optim(rnorm(2), ll_lm, data=df, method = "L-BFGS-B")
coef(lm(bill_depth_mm ~ bill_length_mm, data = df))
