# Template for metropolis hastings algorithm:

dt <- function(x) {
  if (x <= 0) {
    stop("x has to be greater than 0.")
  }
  return(x^5 * exp(-x))
}

dp <- function(x, xt) {
  dchisq(x, floor(xt + 1))
}

rp <- function(x) {
  rchisq(1, floor(x+1))
}

tmax <- 2000

metropolis_hastings <- function(x_0, t_max, dt, dp, rp) {
  # Perform Metropolis-Hastings sampling of the specified density.
  # 
  # Args:
  #   x_0   starting value.
  #   t_max maximum numer of iterations.
  #   dt    density function from which we want to sample.
  #   dp    proposal density function, should accept 2 arguments:
  #           x:  value at which to compute density.
  #           x_t: value on which the rq is conditioned.
  #   rp    proposal random number generator, should accept 1 argument: 
  #           x_t: value on which the rq is conditioned.
  #           
  # Returns:
  #   Vector of sampled points of length t_max. 
  
  x_t <- x_0
  x <- vector("numeric", t_max) # pre-allocate
  
  # Metropolis-Hastings
  for (t in 1:t_max) {
    # Generate a candidate
    y <- rp(x_t)
    # Generate U
    u <- runif(1, 0, 1)
    # Compute alpha
    alpha <- min(1, (dt(y) * dp(x_t, y)) / (dt(x_t) * dp(y, x_t)))
    # Accept the jump or stay in x_t
    if (u < alpha) {
      x_t <- y
    }
    # Sace x_t in vector
    x[t] <- x_t
  }
  
  return(x)
}


# test <- metropolis_hastings(1, 10000, dt, dp, rp)
# hist(test, breaks = 25, freq = FALSE,
#      col = "grey", 
#      xlim = c(0, 20), ylim = c(0, 0.2),
#      xlab = "x", ylab = "Density",
#      main = "Test of metropolis hastings")
# legend("topright",
#        legend = c("Sampled", "True values"),
#        col = c("grey", "red"),
#        pch = c(16, 16))
# lines(x, y / 120, col = "red", lwd = 2)


