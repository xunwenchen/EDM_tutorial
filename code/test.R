# Required Libraries
library(deSolve)

# Define the GLV model function
glv_model <- function(t, populations, params) {
  with(as.list(c(populations, params)), {
    dx1 <- r1 * x1 * (1 - x1 / K1) + a11 * x1^2 + a12 * x1 * x2
    dx2 <- r2 * x2 * (1 - x2 / K2) + a21 * x1 * x2 + a22 * x2^2
    list(c(dx1, dx2))
  })
}

# Set parameters for species interaction
params <- c(r1 = 0.1, r2 = 0.1, K1 = 10, K2 = 10, a11 = -0.01, a12 = 0.02, a21 = -0.02, a22 = -0.01)
initial_populations <- c(x1 = 5, x2 = 5)
time <- seq(0, 200, by = 0.1)

# Solve the GLV model
output <- ode(y = initial_populations, times = time, func = glv_model, parms = params)

# Convert the result to a data frame
output_df <- as.data.frame(output)

# Plot the dynamics of populations
plot(output_df$time, output_df$x1, type = "l", col = "blue", ylim = range(output_df[,-1]),
     xlab = "Time", ylab = "Population", main = "GLV Model: Chaotic Dynamics")
lines(output_df$time, output_df$x2, col = "red")
legend("topright", legend = c("Species 1", "Species 2"), col = c("blue", "red"), lty = 1)
