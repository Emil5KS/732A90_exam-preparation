# Template for the acceptance rejection method:

dtarget <- function(x) {
  dbeta(x, 1, 2)
}

dproposal <- function(x) {
  dunif(x)
}

rproposal <- function() {
  runif(1)
}

maxiter <- 2000
c <- 2

acceptance_rejection <- function(maxiter, c, dt, dp, rp) {
  # Draws from the target distribution by applying acceptance
  # rejection model
  # Args:
  #   maxiter   No. of elements drawn
  #   c         majorizing constant
  #   dt        target density distribution 
  #   dp        proposal density distribution
  #   rp        proposal random draw
  
  X <- vector("numeric", length= maxiter)
  i <- 0
  counter <- 0
  
  repeat {
    Y <- rproposal()
    U <- runif(1)
    
    if (U <= (dtarget(Y) / (c * dproposal(Y)))) {
      X[i] <- Y
      i = i + 1
    }
    
    counter = counter +1
    
    if (i == maxiter) {break}
  }
  
  return(structure(list(
    expected_rejection = (c-1)/c,
    real_rejection = 1 - maxiter/counter,
    X = X
  )))
}
