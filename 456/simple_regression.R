library(ggplot2)
library(dplyr)

df <- read.csv("https://raw.githubusercontent.com/roualdes/data/refs/heads/master/penguins.csv")

ggplot(data = df, aes(body_mass_g, flipper_length_mm)) + 
  geom_point()

fit <- lm(flipper_length_mm ~ body_mass_g, data = df)
summary(fit)

confint(fit, level = 0.98)

predict(fit)
predict(fit, newdata = data.frame(body_mass_g = 4000), interval = "confidence", level = 0.95)

