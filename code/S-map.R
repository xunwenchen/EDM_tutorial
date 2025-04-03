# S-map --------------------------------------------------------------
# Since we've already found E = 2 for both x and y (see simplex.R), we can now apply S-map using E = 2 directly.
# check x and y first
head(d)
# S-map for x
library(rEDM)
sm.rs.x <- SMap(dataFrame = d, 
                lib = "1 100", pred = "101 170", 
                E = 2,
                Tp = 1,
                theta = 10,
                # theta is neighbor localisation exponent
                # choosing the right theta is important
                # see later codes for finding the best theta
                columns = 'X', target = 'X', 
                showPlot = T,
                verbose = T,
                parameterList = T)

plot(sm.rs.x$predictions$Observations, 
     sm.rs.x$predictions$Predictions, 
     xlab = "X Observations", 
     ylab = "X Predictions",
     # add correlation rho in plot
     main = paste("Pearson's rho: ", 
                  round(cor(sm.rs.x$predictions$Observations, 
                            sm.rs.x$predictions$Predictions, 
                            use = "complete.obs"), 10)))


# S-map for y
sm.rs.y <- SMap(dataFrame = d, 
                lib = "1 100", pred = "101 170", 
                E = 2, 
                Tp = 1, 
                theta = 10,
                columns = 'Y', target = 'Y', 
                showPlot = T,
                verbose = T,
                parameterList = T)

plot(sm.rs.y$predictions$Observations, sm.rs.y$predictions$Predictions, 
     xlab = "Y Observations", 
     ylab = "Y Predictions",
     # add correlation rho in plot
     main = paste("Pearson's rho: ", round(cor(sm.rs.y$predictions$Observations, sm.rs.y$predictions$Predictions, use = "complete.obs"), 10)))


# find the best theta ---- NOT WORKING

# Define a range of theta values to test
theta_values <- seq(0.1, 20, by = 0.5)

# Perform parameter sweep and collect results
results <- lapply(theta_values, function(theta) {
  sm <- SMap(dataFrame = d, lib = "1 100", pred = "101 170", E = 2, columns = 'X', target = 'X', theta = theta)
  # Handle cases where 'sm$stats' might be NULL
  if (!is.null(sm$stats)) {
    return(data.frame(theta = theta, rho = sm$stats$rho, RMSE = sm$stats$RMSE))
  } else {
    return(data.frame(theta = theta, rho = NA, RMSE = NA)) # Handle empty outputs
  }
})

# Combine results into a single data frame
results_df <- do.call(rbind, results)

# Plot theta vs. correlation (rho)
ggplot(results_df, aes(x = theta, y = rho)) +
  geom_line(color = "blue") +
  labs(title = "Performance vs Theta (Correlation)", x = "Theta", y = "Correlation (rho)") +
  theme_minimal()

# Plot theta vs. RMSE
ggplot(results_df, aes(x = theta, y = RMSE)) +
  geom_line(color = "red") +
  labs(title = "Performance vs Theta (RMSE)", x = "Theta", y = "RMSE") +
  theme_minimal()




