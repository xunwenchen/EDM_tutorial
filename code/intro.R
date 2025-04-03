# Robert May's logistic map: x(t+1) = a * x(t) * (1 - x(t))
# Define the parameters
a <- 3.5 # Growth rate
x <- seq(0, 1, length.out = 100)  # Sequence of population values (0 to 1)

# Calculate the next population values
x_next <- a * x * (1 - x)

# Plot the equation
plot(x, x_next, type = "l", col = "blue", lwd = 2, xlab = "x(t)", ylab = "x(t+1)",
     main = "x(t+1) = a*x(t)*(1-x(t))")
abline(0, 1, lty = 2)  # Add a reference line (diagonal)

# plot x(t+2) vs x(t)
x_next2 <- a * x_next * (1 - x_next) # iterate once
plot(x, x_next2, type = "l", col = "red", lwd = 2, xlab = "x(t)", ylab = "x(t+2)",
     main = "x(t+1) = a*x(t)*(1-x(t))")
abline(0, 1, lty = 2)  # Add a reference line (diagonal)

# plot x(t+3) vs x(t)
x_next3 <- a * x_next2 * (1 - x_next2) # iterate twice
plot(x, x_next3, type = "l", col = "green", lwd = 2, xlab = "x(t)", ylab = "x(t+3)",
     main = "x(t+1) = a*x(t)*(1-x(t))")
abline(0, 1, lty = 2)  # Add a reference line (diagonal)

# plot x(t+4) vs x(t)
x_next4 <- a * x_next3 * (1 - x_next3) # iterate three times
plot(x, x_next4, type = "l", col = "purple", lwd = 2, xlab = "x(t)", ylab = "x(t+4)",
     main = "x(t+1) = a*x(t)*(1-x(t))")



