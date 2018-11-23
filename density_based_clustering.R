# Density Based Clustering
set.seed(0)
library(dbscan)
data("DS3")
dbs_b <- dbscan(DS3, eps = 11, minPts = 25)
dbs_b #  six 
dbs_b$cluster[1:30] #  0 is an outlier

dbs_c <- dbscan(DS3, eps = 10, minPts = 25)
dbs_c

dbs_d <- dbscan(DS3, eps = 9, minPts = 25)
dbs_d

dbs_e <- dbscan(DS3, eps = 11, minPts = 21)
dbs_e

dbs_f <- dbscan(DS3, eps = 11, minPts = 29)
dbs_f

par(mfrow = c(3,2))
color_1to8 <- function(x) ifelse(x==0,1,((x-1)%%7)+2)
plot(DS3, pch=19, cex=0.5, main="(a) DS3 Data from the 'dbscan' Library", xlab="x1", ylab="x2")
plot(DS3, pch=19, cex=0.5, col=color_1to8(dbs_b$cluster), main="(b) DBSCAN DS3 data [minpts = 25, eps = 11]: 6 clusters", xlab="x1", ylab="x2")
plot(DS3, pch=19, cex=0.5, col=color_1to8(dbs_c$cluster), main="(c) DBSCAN DS3 data [minpts = 25, eps = 10]: 8 clusters", xlab="x1", ylab="x2")
plot(DS3, pch=19, cex=0.5, col=color_1to8(dbs_d$cluster), main="(d) DBSCAN DS3 data [minpts = 25, eps = 9]: 21 clusters", xlab="x1", ylab="x2")
plot(DS3, pch=19, cex=0.5, col=color_1to8(dbs_e$cluster), main="(e) DBSCAN DS3 data [minpts = 21, eps = 11]: 6 clusters", xlab="x1", ylab="x2")
plot(DS3, pch=19, cex=0.5, col=color_1to8(dbs_f$cluster), main="(f) DBSCAN DS3 data [minpts = 29, eps = 11]: 6 clusters", xlab="x1", ylab="x2")

# dat
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

dbs_g <- dbscan(dat, eps = 2.8, minPts = 15)
dbs_g
plot(dat, pch=19, cex=0.5, col=color_1to8(dbs_g$cluster), main = "DBSCAN Data Set Fig. 1 [minpts = 15, eps =2.8]: 4 clusters",
     xlab="x1", ylab="x2")

# HDBSCAN
hdbs <- hdbscan(DS3, minPts = 25)
plot(hdbs)

# FOSC
hdbs <- hdbscan(iris[1:4], minPts = 5)
hdbs
plot(hdbs$hc, main="HDBSCAN* Hierarchy", xlab = "", sub = "", hang = -1, labels = iris$Species, cex = 0.6)
plot(hdbs) #  simplied cluster tree

# dat
hdbs <- hdbscan(dat, minPts = 10)
plot(hdbs)
hdbs
plot(dat, pch=19, cex=0.5, col=color_1to8(hdbs$cluster), main="HDBSCAN* Data Set Fig. 1 (minpts = 10): 4 clusters", xlab="x1", ylab="x2")





