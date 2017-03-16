x <- rnorm(10000, 0, 1)

# Create a histogram plot
hist(x,                                   # data you want to plot
     freq = FALSE,                        # Density plot or count
     breaks = 50,                         # No. of bins
     xlim = c(-4, 4), ylim = c(0, 0.5),   # Limits of x and y
     col = "grey",                        # Color of the bars
     xlab = "x", ylab = "Density",        # Text of x and y axis
     main = "Some histogram")             # Title

# Add an optional density line
lines(density(x),                         # data you want to plot
      col = "red",                        # color
      lwd = 2)                            # linewidth

# Add an optional legend
legend(x = 3, y = 0.5,                    # Position of the legend
       legend = c("data", "density"),     # Names of the entires
       col = c("grey", "red"),            # Colors of the entries
       pch = c(16, 16),                   # Type of point or line
       cex = 0.8)                         # Size