library(ggplot2)
library(dplyr)

df <- read.csv("https://raw.githubusercontent.com/roualdes/data/refs/heads/master/penguins.csv")

ggplot(data = df, aes(island, body_mass_g)) + 
  geom_boxplot() 

dfs <- df |> # pipe operator from R
  group_by(island) %>% # pipe operator from dplyr (library dplyr loads)
  summarise(mn = mean(body_mass_g, na.rm=TRUE),
            sd = sd(body_mass_g, na.rm=TRUE),
            n = sum(!is.na(body_mass_g)),
            lb = mn - qnorm(0.975) * sd / sqrt(n),
            ub = mn + qnorm(0.975)* sd / sqrt(n))

ggplot() +
  #geom_jitter(data = df, aes(island, body_mass_g)) +
  geom_point(data = dfs, aes(island, mn)) +
  geom_linerange(data = dfs, aes(island, ymin = lb, ymax=ub)) +
  geom_abline(intercept = ybar)

fit <- lm(body_mass_g ~ island, data = df) # linear model (just like linear regression)
anova(fit)

# re-creating the ANOVA table, by uh hand.
y <- fit$model$body_mass_g
(m <- dfs$mn)
(n <- dfs$n)
(ybar <- mean(y))
(yhat <- predict(fit))
(SSR <- sum((y - yhat) ^ 2)) # sum of squares residual
(SSA <- sum(n * (m - ybar) ^ 2)) # sum of squares within/amongst
(MSA <- SSA / 2) # mean squares within/amongst
(MSR <- SSR / 339) # mean squares residuals
(F <- MSA / MSR) # F test statistic
1 - pf(F, 2, 339) # p-value
