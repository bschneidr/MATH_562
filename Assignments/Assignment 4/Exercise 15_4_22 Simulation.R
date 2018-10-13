library(ggplot2)

# Probability density function
  density_fn <- function(x, lambda) {
    
    density <- vector(mode = 'numeric', length = length(x))
    
    for (i in seq_along(x)) {
      if (x[i] > 1 | x[i] < 0) {
        density[i] <- 0
      } else {
        density[i] <- 1 - (2/3)*lambda + lambda*(sqrt(x[i]))
      }
    }
    density
  }

# The two observations
  observations <- c(1/4, 9/16)

# The likelihood function for lambda, given the observations
  likelihood <- function(lambda, data = observations) {
    density <- vector(mode = 'numeric', length = length(lambda))
    for (i in seq_along(lambda)) {
      densities <- sapply(X = data,
                          FUN = function(x) density_fn(x,
                                                       lambda[i])
      )
      density[i] <- prod(densities)
    }
    density
  }

# Plot values of the likelihood function
# across the parameter space (i.e. the interval [-3, 3/2])
  
  ggplot(data.frame(x = c(-8, 8)),
                    aes(x)) +
    stat_function(fun = likelihood,
                  args = list(data = observations),
                  geom = "line") +
    geom_vline(xintercept = -3) +
    scale_x_continuous("Lambda", breaks = seq(-9, 9, 3)) +
    scale_y_continuous("Likelihood") +
    theme_minimal()
