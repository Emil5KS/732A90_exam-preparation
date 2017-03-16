calculate_conditional_probability <- function(i, X, Y, ...) {
  # Returns the conditional probability of an element
  # Args: (might change with the given task)
  #   X   Value we are looking for
  #   Y   Data
  #   i   position
  # In the lab we had to consider three cases 
  # This will change in the exam!
  d <- length(X)
  if (i == 1) {
    # i = 1
    mean <- (Y[1] + X[2]) / 2
    variance <- sigma_squared / 2
  } else if (i == d) {
    # i = d
    mean <- (X[d - 1] + Y[d]) / 2
    variance <- sigma_squared / 2
  } else { 
    # i = 2 ... d-1
    mean <- (X[i - 1]+ Y[i] + X[i + 1]) / 3
    variance <- sigma_squared / 3
  }
  
  # Generate a random variables
  X_i <- rnorm(1, mean = mean, sd = sqrt(variance))
  
  return(X_i)
}

gibbs_sampling <- function(Y, n, X0, ...) {
  # Gibbs sampling
  # Args:
  #   Y   Data
  #   n   Number of generated samples
  #   X0  Initialization points
  # Returns:
  #   X   Matrix with samples (colmeans = value we are looking for)
  d <- length(Y)
  X <- matrix(NA, ncol = d, nrow = n)
  X[1, ] <- X0
  
  for (row in 2:n) {
    for (col in 1:d) {
      X[row, col] <- calculate_conditional_probability(
        i = col,
        X = c(X[row, 1:(col - 1)], X[row - 1, col:d]),
        Y = Y,
        sigma_squared = sigma_squared
      )
    }
  }
  return(X)
}
