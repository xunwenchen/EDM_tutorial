# Simplex ---- 
# Predict the future of the time series
# Use the first 100 points to predict the next 70 points
# The obtained results contain the 70 observations and 70 predictions
# Assume E = 2 as an example but we can find the best E by E = 1:10 (see later codes)
smplx.rs <- Simplex(dataFrame = d, lib = "1 100", pred = "101 170", E = 2, columns = 'X', target = 'X')

# plot observations vs predictions, and add variance
plot(smplx.rs$Observations, smplx.rs$Predictions, 
     xlab = "X Observations", 
     ylab = "X Predictions")

# do the same for Y
smplx.rs.y <- Simplex(dataFrame = d, lib = "1 100", pred = "101 170", E = 2, columns = 'Y', target = 'Y')

# plot observations vs predictions, and add variance
plot(smplx.rs.y$Observations, smplx.rs.y$Predictions, 
     xlab = "Y Observations", 
     ylab = "Y Predictions")

# Find best embedding dimension E ----------------------------------------------

# make E = 1: 10 and then correlate observations and predictions and plot Pearson's correlation coefficients with E and omit NA values
E <- 1:10
corrs <- sapply(E, function(e) {
  smplx.rs <- Simplex(dataFrame = d, lib = "1 100", pred = "101 170", E = e, columns = 'X', target = 'X')
  cor(smplx.rs$Observations, smplx.rs$Predictions, use = "complete.obs")
})


# do the same for Y
corrs.y <- sapply(E, function(e) {
  smplx.rs.y <- Simplex(dataFrame = d, lib = "1 100", pred = "101 170", E = e, columns = 'Y', target = 'Y')
  cor(smplx.rs.y$Observations, smplx.rs.y$Predictions, use = "complete.obs")
})

# plot values of E vs correlation coefficients (expression(rho)), and add data points connected with lines using ggplot. X ticks only show integers, find the highest rho then circle it with a red line
ggplot() + geom_line(aes(x = E, y = corrs), color = "darkgray") +
  geom_point(aes(x = E, y = corrs), color = "darkgray") +
  geom_line(aes(x = E, y = corrs.y), color = "indianred") +
  geom_point(aes(x = E, y = corrs.y), color = "indianred") +
  labs(x = "E", y = expression(rho)) +
  scale_x_continuous(breaks = E) 