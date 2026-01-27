x <- c(4, 6, 7, 8, 10) # concatenate
s <- 0
for (i in 1:length(x)) {
  s <- s + x[i]
}
s

t <- 0
for (xi in x) {
  t <- t + xi
}
t

l <- list(a = 1, b = 1.2, c = "edward",  d = TRUE, "words and stuff" = c(3, 4))
l$`words and stuff` <- 3.14
s <- 0
for (i in 1:length(l)) {
  s <- s + l[[i]]
}
s

df <- data.frame(g = c("a", "b", "b", "c", "a", "c"),
                 x = 1:6, 
                 y = 2:7)

library(ggplot2)
ggplot(data = df, aes(g, x)) +
  geom_boxplot()

ggplot(data = df, aes(x, y)) +
  geom_point()

fit <- lm(y ~ x, data = df)
summary(fit)

