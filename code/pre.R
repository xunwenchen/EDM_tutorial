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

# Plotting the time series
p <- ggplot() + 
  geom_line(aes(x = time, y = x), data = d, color = "blue") +
  geom_line(aes(x = time, y = y), data = d, color = "red") +
  xlab("Time") + 
  ylab("Value")
p

# Assume that the best embedding dimension (E) = 2.
y.E <- 2
y.embed <- matrix(NA, nrow = length(y), ncol = y.E)
for(i in 1:y.E) y.embed[i:length(y), i] <- y[1:(length(y)-i+1)]

# Calculating Euclidean distances between the target and other points
set.target <- matrix(rep(y.embed[tl-1,], nrow(y.embed)), ncol = ncol(y.embed)
                     , byrow = T)
distances <- sqrt(rowSums((y.embed - set.target)^2))
neighbors <- order(distances[-(tl-1)])[1:(y.E+1)] # Exclude the target point
