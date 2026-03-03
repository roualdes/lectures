library(ggplot2)
library(dplyr)

data(state)
state <- as.data.frame(state.x77)

# review
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
  geom_point(aes(yhat, r), alpha = 0.1)

# multiple linear regression
fit <- lm(Income ~ Population + `Life Exp` + Illiteracy + Murder + `HS Grad` + Frost + Area, data = state)
summary(fit)

mp <- mean(state$Population)
mi <- mean(state$Illiteracy)
mle <- mean(state$`Life Exp`)
mm <- mean(state$Murder)
mhg <- mean(state$`HS Grad`)
mf <- mean(state$Frost)
ma <- mean(state$Area)

nd <- data.frame(Population = c(mp, mp),
                 Illiteracy = c(mi, mi),
                 `Life Exp` = c(mle, mle + 1), # one unit change in Life Expectancy
                 Murder = c(mm, mm),
                 `HS Grad` = c(mhg, mhg),
                 Frost = c(mf, mf),
                 Area = c(ma, ma),
                 check.names = FALSE # this (and backticks) required if variables have spaces
                 )

diff(predict(fit, newdata = nd))


# cars data set
# linear doesn't mean what you previously might have thought
data(mtcars)

# line doesn't fit data well
ggplot(data=mtcars, aes(wt, mpg)) +
  geom_point() + 
  geom_smooth(se = FALSE, method = "lm")

# look for the non-linearity in the 
# scatter plot: standardized residuals on yhat
fit <- lm(mpg ~ wt, data = mtcars)
mtcars$r <- rstandard(fit)
mtcars$yhat <- predict(fit)

ggplot(data=mtcars) + geom_point(aes(yhat, r))

# add a quadratic term in wt
fit <- lm(mpg ~  wt + I(wt * wt), data = mtcars)
summary(fit)

# the I(...) treats * as multiply
# it's as if we fit the following model
# mtcars$wt2 <- mtcars$wt * mtcars$wt
# fit <- lm(mpg ~  wt + wt2, data = mtcars)
# summary(fit)
# but doing it this way has annoying side effects

o <- order(mtcars$wt)
x <- seq(min(mtcars$wt), max(mtcars$wt), length.out=101)
mtcars <- mtcars |>
  mutate(
    wt = wt[o],
    mpg = mpg[o])

dfr <- data.frame(x = x,
                   yhat = predict(fit, newdata = data.frame(wt = x)))

ggplot() +
  geom_point(data=mtcars, aes(wt, mpg)) + 
 geom_line(data = dfr, aes(x, yhat), color = "blue")

dfr <- data.frame(
  r = rstandard(fit)
)

# we haven't yet solved all problems, but our model does predict better
ggplot(data = dfr) + geom_histogram(aes(r), bins = 11)

# for non-linear models (in the class of linear regression)
# the rate of change in yhat changes dependent on from where
# you make an interpretation; compare to wt = c(2, 3)
diff(predict(fit, newdata = data.frame(wt = c(4, 5))))

# back to state data

# inside lm(...) * means interaction term
# all lower order terms are included, up to and including, 
# the highest order term specified
fit <- lm(Income ~ Population * Area, data = state)
summary(fit)

# non-linear in multiple ways
fit <- lm(log10(mpg) ~ log10(wt), data = mtcars)
summary(fit)

dfl <- data.frame(
  x = x,
  yhat = 10^predict(fit, newdata = data.frame(wt = x))
)

ggplot() +
  geom_point(data = mtcars, aes(wt, mpg)) +
  geom_line(data = dfl, aes(x, yhat), color = "blue")

yhat <- predict(fit)
mean((10^yhat - mtcars$mpg)^2) # Mean Squared Error for mpg; undo log10(mpg)
