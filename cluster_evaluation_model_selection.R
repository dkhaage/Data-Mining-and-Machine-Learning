# Cluster Evaluation and Model Selection
library(MASS)

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

# number of clusters
set.seed(0)
SSE <- rep(0,10)
for (k in 1:10){
  km.out <- kmeans(x = dat, centers = k, nstart = 10)
  SSE[k] <- km.out$tot.withinss
}
plot(1:10, SSE, xlab="k", ylab="SSE", type = "b")


# number of clusters
set.seed(0)
SSE <- rep(1,10)
for (k in 1:10){
  km.out <- kmeans(x = iris[1:4], centers = k, nstart = 10)
  SSE[k] <- km.out$tot.withinss
}
plot(1:10, SSE, xlab="k", ylab ="SSE", type = "b")

# Classifications according to 3 clusters Iris
set.seed(0)
km.out <- kmeans( x = iris[1:4], centers = 3, nstart = 10)
table(km.out$cluster, iris$Species)


# Silhouette Width Criterion (SWC)
SWC <- function(clusterLabels, dataPoints){
  require(cluster)
  sil <- silhouette(x = clusterLabels, dist = dist(dataPoints))
  return(mean(sil[,3]))
}

set.seed(0)
Silhouette <- rep(0,10)
for (k in 2:10){
  km.out <- kmeans(x = dat, centers = k, nstart = 10)
  Silhouette[k] <- SWC(clusterLabels = km.out$cluster, dataPoints = dat)
}
plot(2:10, Silhouette[2:10], xlab="k", ylab="Silhouette Width Criterion (SWC)", type = "b")

# Iris Silhouette
set.seed(0)
Silhouette <- rep(1,10)
for (k in 2:10){
  km.out <- kmeans( x= iris[1:4], centers = k, nstart = 10)
  Silhouette[k] <- SWC(clusterLabels = km.out$cluster, dataPoints = iris[1:4])
}
plot(2:10, Silhouette[2:10], xlab="k", ylab= "Silhouette Width Criterion (SWC)", type = "b")











