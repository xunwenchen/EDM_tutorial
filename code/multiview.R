# Simplex and S-map are primarily used for low-dimensional time series data.
# later we have multivariate embedding method from Dixon et al. (1999) 

# Then, the multiview embedding method is developped by Ye and Sugihara (2016).
# Ye H., and G. Sugihara, 2016. Information leverage in interconnected ecosystems: Overcoming the curse of dimensionality. Science 353:922-925.


# using the data from the rEDM package for illustration
data(block_3sp)
head(block_3sp)

# the data contain time, x, y, and z, and with their -1 and -2 lagged values
# plot x, x_t-1, x_t-2 in 3D using scatterplot3d using line style
library(scatterplot3d)
scatterplot3d(block_3sp$x_t, block_3sp$x_t-1, block_3sp$x_t-2, type = 'l', color = 'red')
scatterplot3d(block_3sp$x_t, block_3sp$y_t, block_3sp$z_t, type = 'l', color = 'blue')


# plot x, x_t-1, x_t-2 over time using plot()
plot(block_3sp$time, block_3sp$x_t, type = 'l', col = 'red')
lines(block_3sp$time, block_3sp$x_t-1, col = 'blue')
lines(block_3sp$time, block_3sp$x_t-2, col = 'green')


# find best embedding dimension E
E = 1:10
corrs = sapply(E, function(e) {
  smplx.rs = Simplex(dataFrame = block_3sp, lib = "1 100", pred = "101 198", E = e, columns = 'x_t', target = 'x_t')
  cor(smplx.rs$Observations, smplx.rs$Predictions, use = "complete.obs")
})

# do the same for y_t
corrs.y = sapply(E, function(e) {
  smplx.rs.y = Simplex(dataFrame = block_3sp, lib = "1 100", pred = "101 198", E = e, columns = 'y_t', target = 'y_t')
  cor(smplx.rs.y$Observations, smplx.rs.y$Predictions, use = "complete.obs")
})

# z_t
corrs.z = sapply(E, function(e) {
  smplx.rs.z = Simplex(dataFrame = block_3sp, lib = "1 100", pred = "101 198", E = e, columns = 'z_t', target = 'z_t')
  cor(smplx.rs.z$Observations, smplx.rs.z$Predictions, use = "complete.obs")
})

# plot
ggplot() + geom_line(aes(x = E, y = corrs), color = "darkgray") +
  geom_point(aes(x = E, y = corrs), color = "darkgray") +
  geom_line(aes(x = E, y = corrs.y), color = "indianred") +
  geom_point(aes(x = E, y = corrs.y), color = "indianred") +
  geom_line(aes(x = E, y = corrs.z), color = "blue") +
  geom_point(aes(x = E, y = corrs.z), color = "blue") +
  labs(x = "E", y = expression(rho)) +
  scale_x_continuous(breaks = E)

# we can see E for x_t, y_t, and z_t are 3, 2, and 4, respectively

# multiview embedding 
# since there are 3 variables, it shall have 9 combinations. So, we can reconstructed 9 views/manifolds?

L = Multiview(dataFrame = block_3sp, 
              lib = "1 100", pred = "101 190",
              E = 3, 
              columns = "x_t y_t z_t", 
              target = "x_t", 
              showPlot = T,
              D = 3, 
              Tp = 1,
              verbose = T,
              parameterList = T,
              numThreads = 10) # my PC is up to 14 cores and 20 threads

plot(L$Predictions$Observations, L$Predictions$Predictions, 
     xlab = "x_t Observations", 
     ylab = "x_t Predictions",
     # add correlation rho in plot
     main = paste("Pearson's rho: ", 
                  round(cor(L$Predictions$Observations,
                            L$Predictions$Predictions, 
                            use = "complete.obs"), 10)))

L$View

