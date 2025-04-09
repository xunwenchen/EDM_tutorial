# preparation ----
# early part of this tutorial is using rEDM package version 1.15.4

detach("package:rEDM", unload = TRUE)
remove.packages("rEDM")
remove.packages("rEDM", lib = "C:/Program Files/R/R-4.4.1/library")


install.packages("rEDM_1.15.4.tar.gz", repos = NULL, type = "source") # install from local file, or we can install directly from CRAN
# install.packages("rEDM") # install from CRAN

library(rEDM)
# check version
packageVersion("rEDM")
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
p <- ggplot(data = d, aes(x = t)) +
  geom_line(aes(y = X, color = "X")) +
  geom_line(aes(y = Y, color = "Y")) +
  geom_point(aes(y = X), color = "darkgray", shape = 1, size = 2) +
  geom_point(aes(y = Y), color = "indianred", shape = 1, size = 2)+
  scale_color_manual(values = c("X" = "darkgray", "Y" = "indianred")) +
  labs(color = "Species", x = "Time", y = "Abundance") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p


