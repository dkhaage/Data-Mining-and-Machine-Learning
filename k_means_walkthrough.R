# K-Means Walkthrough

# We generate four clusters following multivariate normal distributions in two dimensions:
set.seed(0)
x11 <- rnorm(n = 100, mean = 10, sd = 1) # Cluster 1 (x1 coordinate)
x21 <- rnorm(n = 100, mean = 10, sd = 1) # Cluster 1 (x2 coordinate)
x12 <- rnorm(n = 100, mean = 20, sd = 1) # Cluster 2 (x1 coordinate)
x22 <- rnorm(n = 100, mean = 10, sd = 1) # Cluster 2 (x2 coordinate)
x13 <- rnorm(n = 100, mean = 15, sd = 3) # Cluster 3 (x1 coordinate)
x23 <- rnorm(n = 100, mean = 25, sd = 3) # Cluster 3 (x2 coordinate)
x14 <- rnorm(n = 50, mean = 25, sd = 1)  # Cluster 4 (x1 coordinate)
x24 <- rnorm(n = 50, mean = 25, sd = 1)  # Cluster 4 (x2 coordinate)
dat <- data.frame(x1 = c(x11,x12,x13,x14), x2 = c(x21,x22,x23,x24))
plot(dat$x1, dat$x2, xlim = c(5,30), ylim = c(5,35), xlab = "x1", ylab = "x2")

# We can run k-means with k=4 in this dataset using the function kmeans() from the stats package available in base R:
library(stats)

set.seed(0)
km.out <- kmeans(x = dat, centers = 4)
km.out

# The result can be visualised by mapping the cluster labels into colours:
plot(dat, col=(km.out$cluster+1), main="k=4: single initialisation with set.seed(0)", xlab="x1", ylab="x2")

# Notice that the result does not correspond to the four clusters as they have been generated. (Random Initialization Trap)

# The reason is that the algorithm got trapped into a local minima, as described earlier. 
# We can confirm this by running the algorithm again from a different random initialisation, 
# and noticing that the result will be very different:
set.seed(2)
km.out <- kmeans(x = dat, centers = 4)
km.out$tot.withinss

plot(dat, col=(km.out$cluster+1), main="k=4: single initialisation with set.seed(2)", xlab="x1", ylab="x2")

# The function kmeans() provides a built-in implementation of this procedure, 
# activated by setting attribute nstart to an integer greater than 1 (nstart = 1 is the default value, 
# which corresponds to a single initialisation). Let's try nstart = 10 in our example,
# which means running k-means 10 times and then choose the solution with least SSE value:

set.seed(0)
km.out <- kmeans(x = dat, centers = 4, nstart = 10)
km.out$tot.withinss

plot(dat, col=(km.out$cluster+1), main="k=4: best out of 10 initialisations", xlab="x1", ylab="x2")
# Notice that the solution above is visually much better,
# and the corresponding SSE is less than half the ones we had obtained from the two previous individual initialisations.

# nstart 1 through 50
nstart <- seq(from=0, to=50, by=5); nstart[1] <- 1
SSE <- rep(0, 11)
for (i in 1:11){
  set.seed(0)
  km.out <- kmeans(x = dat, centers = 4, nstart = nstart[i])
  SSE[i] <- km.out$tot.withinss
}
plot(nstart, SSE, xlab="nstart", ylab="SSE")







