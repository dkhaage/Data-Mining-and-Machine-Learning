# Unsupervised Outlier Detection - KNN Outlier 

library(ggplot2)
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
( g0a <- ggplot() + geom_point(data=dat, mapping=aes(x=x1, y=x2), shape = 19) )

library(dbscan)
k <- 4 # KNN parameter
KNN_Outlier <- kNNdist(x=dat, k = k)[,k] # KNN distance (outlier score) computation
# kNNdist(x=dat, k = 4)[i,]  returns the distances of the ith observation to its 1st, 2nd, . and kth nearest neighbours,

# sorts the observations according to their KNN outlier scores and displays the top 20 outliers along with their scores:
top_n <- 20 # No. of top outliers to be displayed
rank_KNN_Outlier <- order(x=KNN_Outlier, decreasing = TRUE) # Sorting (descending)
KNN_Result <- data.frame(ID = rank_KNN_Outlier, score = KNN_Outlier[rank_KNN_Outlier])
head(KNN_Result, top_n)

# We can see that the most outlying observation is observation 237, with outlier score = 4.42,
# followed by observation 208 with outlier score = 4.17, 
# and so on. Since this dataset is 2D, we can visualise the top 20 outliers (highlighted in red) alongside with their IDs
g <- g0a +
  geom_point(data=dat[rank_KNN_Outlier[1:top_n],], mapping=aes(x=x1,y=x2), shape=19, color="red", size=2) +
  geom_text(data=dat[rank_KNN_Outlier[1:top_n],],
            mapping=aes(x=(x1-0.5), y=x2, label=rank_KNN_Outlier[1:top_n]), size=2.5)
g
# KNN outlier (k=4): Red points are the top 20 outliers, with their IDs

g0b <- ggplot() + geom_point(data=dat, mapping=aes(x=x1, y=x2), shape = 19, size = 0.1)
g2 <- g0b +
  geom_point(data=dat, mapping=aes(x=x1, y=x2, size = KNN_Outlier), shape = 1, color = "red") +
  scale_size_continuous(range = c(0.1, 20))

g0b
g2
# KNN outlier (k=4): Circle radii are proportional to outlier scores
# shows that, as a global outlier detection method, 
# KNN may have difficulty detecting local outliers when there are clusters of varied densities in the data.

# WEIGHTED KNN OUTLIER

k <- 4 # KNN parameter
top_n <- 20 # No. of top outliers to be displayed
WKNN_Outlier <- apply(kNNdist(x=dat, k = k), 1, mean) # Weighted KNN outlier score computation
rank_WKNN_Outlier <- order(x=WKNN_Outlier, decreasing = TRUE) # Sorting (descending)
WKNN_Result <- data.frame(ID = rank_WKNN_Outlier, score = WKNN_Outlier[rank_WKNN_Outlier])
head(WKNN_Result, top_n)

ge1 <- g0a +
  geom_point(data=dat[rank_WKNN_Outlier[1:top_n],], mapping=aes(x=x1,y=x2), shape=19, color="red", size=2) +
  geom_text(data=dat[rank_WKNN_Outlier[1:top_n],],
            mapping=aes(x=(x1-0.5), y=x2, label=rank_WKNN_Outlier[1:top_n]), size=2.5)
ge1

ge2 <- g0b +
  geom_point(data=dat, mapping=aes(x=x1, y=x2, size = WKNN_Outlier), shape = 1, color = "red") +
  scale_size_continuous(range = c(0.1, 20))
ge2

# PAGE BLOCKS DATASET
# https://archive.ics.uci.edu/ml/datasets/Page+Blocks+Classification

PageBlocks <- read.table("page-blocks.csv", header=FALSE, sep="", dec=".")
str(PageBlocks) # 10 Predictors (V1 to V10) and class labels (V11)

PB_Predictors <- PageBlocks[,1:10] # 10 Predictors (V1 to V10)
PB_class <- PageBlocks[,11] # Class labels (V11)
PB_class <- ifelse(PB_class == 1,0,1) # Inliers (class "1") = 0, Outliers (classes "2", "3", "4", "5") = 1

# Original (Non-Normalised) Data:
summary(PB_Predictors)

P_at_n <- rep(0, 50)
for(k in 1:50){
  KNN_Outlier <- kNNdist(x=PB_Predictors, k = k)[,k]
  rank_KNN_Outlier <- order(x=KNN_Outlier, decreasing = TRUE) # Sorting (descending)
  P_at_n[k] <- mean(PB_class[rank_KNN_Outlier[1:560]] == 1) # Precision at n
}
plot(1:50, P_at_n)

# Normalised Data:
PB_Predictors <- scale(PageBlocks[,1:10])
summary(PB_Predictors)

P_at_n <- rep(0, 50)
for(k in 1:50){
  KNN_Outlier <- kNNdist(x=PB_Predictors, k = k)[,k]
  rank_KNN_Outlier <- order(x=KNN_Outlier, decreasing = TRUE) # Sorting (descending)
  P_at_n[k] <- mean(PB_class[rank_KNN_Outlier[1:560]] == 1) # Precision at n
}
plot(1:50, P_at_n)

# if all the 560 true outliers are ranked above the inliers according to the outlier scores, the precision@n measure will be 1. 
# In the other extreme, if the top 560 observations according to the outlier scores are actually inliers according to the ground truth,
# then precision@n will be zero.

