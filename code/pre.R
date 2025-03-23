# preparation ----
library(rEDM)
library(ggplot2)
# set theme
theme_set(theme_bw())


# following codes are from Ushio M, Kawatsu K (2020) Forecasting Ecological Time Series Using Empirical Dynamic Modeling: A Tutorial for Simplex Projection and S-map. In: Mougi A (ed) Diversity of Functional Traits and Interactions: Perspectives on Community Dynamics. Springer, Singapore, pp 193–213

# Load model time series
d <- read.csv("data/TwoSppUni_Example.csv")
tl <- nrow(d)
time <- d$t
# Usually data should be normalized (e.g., using scale() function) before EDM.
# Here we do not perform normalization just for better visualization.
x <- d$X
y <- d$Y

# Plotting the time series with lines and points
p <- ggplot() + geom_line(aes(x = time, y = x), color = "darkgray") +
  geom_point(aes(x = time, y = x), color = "darkgray") +
  geom_line(aes(x = time, y = y), color = "indianred") +
  geom_point(aes(x = time, y = y), color = "indianred") +
  labs(x = "Time", y = "Value") +
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p

data(block_3sp)

# use Simplex function to predict the future of the time series
# use the first 100 points to predict the next 70 points
# the obtained results contain the 70 observations and 70 predictions
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


# Assume that the best embedding dimension (E) = 2.
y.E <- 2
y.embed <- matrix(NA, nrow = length(y), ncol = y.E)
for(i in 1:y.E) y.embed[i:length(y), i] <- y[1:(length(y)-i+1)]

# Calculating Euclidean distances between the target and other points
set.target <- matrix(rep(y.embed[tl-1,], nrow(y.embed)), ncol = ncol(y.embed)
                     , byrow = T)
distances <- sqrt(rowSums((y.embed - set.target)^2))
neighbors <- order(distances[-(tl-1)])[1:(y.E+1)] # Exclude the target point
