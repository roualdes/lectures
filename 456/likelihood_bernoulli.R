
ll_bernoulli <- function(p, data) {
  x <- data$x
  sx <- sum(x)
  N <- length(x)
  -sx * log(p) + (sx - N) * log(1-p) # negative, simplified, log-likelihood
}

x <- rbinom(100, 1, 0.11)

# left hand side x is the name of the column
# right hand side x from the global scope
df <- data.frame(x = x)

optim(runif(1), # passed in to first argument off ll_bernoulli
      # could also use any number within (lower, upper)
      ll_bernoulli, # function to minimize
      data = df,  # data frame with x in it, 
      # passed into second argument of ll_bernoulli
      method = "L-BFGS-B", 
      lower = 1e-5, # bounds on first argument of ll_bernoulli,
      upper = 1 - 1e-5)

# optim should return the same as
mean(x)
