# rEDM intro ----

# rEDM is a Rcpp interface to the cppEDM library of Empirical Dynamic Modeling tools. Functionality includes:
   
# Simplex projection (Sugihara and May 1990)
# Sequential Locally Weighted Global Linear Maps (S-map) (Sugihara 1994)
# Multivariate embeddings (Dixon et. al. 1999)
# Convergent cross mapping (Sugihara et. al. 2012)
# Multiview embedding (Ye and Sugihara 2016)

# References
# Sugihara G. and May R. 1990. Nonlinear forecasting as a way of distinguishing chaos from measurement error in time series. Nature, 344:734-741.

# Sugihara G. 1994. Nonlinear forecasting for the classification of natural time series. Philosophical Transactions: Physical Sciences and Engineering, 348 (1688) : 477-495.
 
# Dixon, P. A., M. Milicich, and G. Sugihara, 1999. Episodic fluctuations in larval supply. Science 283:1528-1530.

# Sugihara G., May R., Ye H., Hsieh C., Deyle E., Fogarty M., Munch S., 2012. Detecting Causality in Complex Ecosystems. Science 338:496-500.
 
# Ye H., and G. Sugihara, 2016. Information leverage in interconnected ecosystems: Overcoming the curse of dimensionality. Science 353:922-925.


# Simplex --------------------------------------------------------------
# Predict the future of the time series
# Use the first 100 points to predict the next 70 points
# The obtained results contain the 70 observations and 70 predictions
# Assume E = 2 as an example 
# but we can find the best E by E = 1:10 (see later codes)
library(rEDM)
smplx.rs <- Simplex(dataFrame = d, 
                    lib = "1 100", pred = "101 170", 
                    E = 2, Tp = 1, 
                    columns = 'X', target = 'X', 
                    showPlot = T)
# in the output figure, Tp = 1 (Prediction Horizon/Time prediction) means that the model is predicting one time step ahead based on the input data.


# plot observations vs predictions, and add variance
plot(smplx.rs$Observations, smplx.rs$Predictions, 
     xlab = "X Observations", 
     ylab = "X Predictions")
# correlation between observations and predictions, remove NA values
cor(smplx.rs$Observations, smplx.rs$Predictions, use = "complete.obs")

# do the same for Y
smplx.rs.y <- Simplex(dataFrame = d, lib = "1 100", pred = "101 170", E = 2, Tp = 1, columns = 'Y', target = 'Y', showPlot = T)


# plot observations vs predictions, and add variance
plot(smplx.rs.y$Observations, smplx.rs.y$Predictions, 
     xlab = "Y Observations", 
     ylab = "Y Predictions")
# correlation between observations and predictions, remove NA values
cor(smplx.rs.y$Observations, smplx.rs.y$Predictions, use = "complete.obs")

# ~~ Find best embedding dimension E ----------------------------------------------
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

# The figure shows when rho is the highest, which is the best E value for the time series, E = 2 for both x and y.



