# Hierarchial Clustering

# SINGLE-LINKAGE ALGORITHM 
library(ggplot2)
x1 <- c(1,2,3,4,8,9,10)
x2 <- c(6,1,1,1.5,4.5,5,5)
Point_ID <- as.factor(c(1:7))
dat_7points <- data.frame(x1 = x1, x2 = x2, Point_ID = Point_ID)
( g <- ggplot(data = dat_7points) + geom_point(mapping = aes(x=x1, y=x2), size = 3) +
    geom_text(mapping = aes(x=(x1-0.2), y=x2, label = Point_ID)))

library(stats)
# hclust() from stats for AHC clustering. SL method chosen by setting attribute methode to 'single'

DistMatrix <- dist(dat_7points[1:2])
SL_7points <- hclust(DistMatrix, method = "single")

# represent results as a tree using cutree()
cutree(tree = SL_7points, k = 1:7)

# dendrogram (upside down tree)
plot(SL_7points, main = "Single Linkage", xlab = "", sub = "", hang = -1)

# SL dendrogram iris
DMatrix <- dist(iris[1:4])
SL_iris <- hclust(DMatrix, method = "single")
plot(SL_iris, main = "Single Linkage", xlab = "", sub = "", hang = -1, labels = FALSE)
plot(SL_iris, main = "Single Linkage", xlab = "", sub = -1, labels = iris$Species, cex = 0.6) # Labels

# New Example
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

# SL dendrogram for dat
datMat <- dist(dat)
SL_4Gauss <- hclust(datMat, method = "single")
plot(SL_4Gauss, main = "Single Linkeage", xlab = "", sub = "", hang = -1, labels = FALSE)

# COMPLETE LINKAGE ALGORITHM

# dendrogram for CL
CL_7points <- hclust(DistMatrix, method = "complete")
plot(CL_7points, main = "Complete Linkage", xlab = "", sub = "", hang = -1)

# CL for dat
datMat <- dist(dat)
CL_4Gauss <- hclust(datMat, method = "complete")
plot(CL_4Gauss, main = "Complete Linkage", xlab = "", sub = "", hang = -1, labels = FALSE)

# CL for iris
DMatrix <- dist(iris[1:4])
CL_iris <- hclust(DMatrix, method = "complete")
plot(CL_iris, main = "Complete Linkage", xlab = "", sub = "", hang = -1, labels = FALSE)

# AVERAGE LINKAGE
D <- as.matrix(dist(dat_7points[1:2]))
( (D[2,5]+D[2,6]+D[2,7]+D[3,5]+D[3,6]+D[3,7]+D[4,5]+D[4,6]+D[4,7])/9)
( (D[2,1]+D[3,1]+D[4,1])/3 )
( (D[5,1]+D[6,1]+D[7,1])/3 )

# AL dendrogram 7 points
AL_7points <- hclust(DistMatrix, method = "average")
plot(AL_7points, main = "Average Linkage", xlab = "", sub ="", hang = -1)

# AL dendrogram dat
datMat <- dist(dat)
AL_4Gauss <- hclust(datMat, method = "average")
plot(AL_4Gauss, main = "Average Linkage", xlab = "", sub ="", hang = -1, labels= FALSE )

# AL dendrogram iris
DMatrix <- dist(iris[1:4])
AL_iris <- hclust(DMatrix, method = "average")
plot(AL_iris, main = "Average Linkage", xlab = "", sub = "", hang =-1, labels = FALSE)

# WARDS ALGORITHM (resembles k-means)

# Ward 7 points
Ward_7points <- hclust(DistMatrix, method = "ward.D2")
plot(Ward_7points, main = "Ward's", xlab = "", sub = "", hang = -1)

# Ward dat
datMat <- dist(dat)
Ward_4Gauss <- hclust(datMat, method = "ward.D2")
plot(Ward_4Gauss, main = "Ward's", xlab = "", sub = "", hang = -1, labels = FALSE)

# Ward iris
DMatrix <- dist(iris[1:4]) #  Euclidean distances must be provided so that Ward's has a mathematical interpretation 
# (the y are squared internally in option "word.D2)
Ward_iris <- hclust(DMatrix, method = "ward.D2") #  "ward.D2" implements the origional Ward's (1963) criterion,
# where the dissimilarities are squared before cluster updating
plot(Ward_iris, main = "Ward's", xlab = "", sub = "", hang = -1, labels = FALSE)
















