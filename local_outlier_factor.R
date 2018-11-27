# Local Outlier Factor (LOF)
library(ggplot2)
library(dbscan)
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

k <- 4 # LOF parameter
LOF_Outlier <- lof(x=dat, k = k) # LOF (outlier score) computation

top_n <- 20 # No. of top outliers to be displayed
rank_LOF_Outlier <- order(x=LOF_Outlier, decreasing = TRUE) # Sorting (descending)
LOF_Result <- data.frame(ID = rank_LOF_Outlier, score = LOF_Outlier[rank_LOF_Outlier])
head(LOF_Result, top_n)

g3 <- g0a +
  geom_point(data=dat[rank_LOF_Outlier[1:top_n],],mapping=aes(x=x1, y=x2),shape=19,color="red",size=2) +
  geom_text(data=dat[rank_LOF_Outlier[1:top_n],],
            mapping=aes(x=(x1-0.5), y=x2, label=rank_LOF_Outlier[1:top_n]), size=2.5)
g3

g4 <- g0a +
  geom_point(data=dat, mapping=aes(x=x1, y=x2, size = LOF_Outlier), shape = 1, color = "red") +
  scale_size_continuous(range = c(0.1, 20))
g4

# PAGE BLOCKS
PageBlocks <- read.table("page-blocks.csv", header=FALSE, sep="", dec=".")
str(PageBlocks) # 10 Predictors (V1 to V10) and class labels (V11)

PB_Predictors <- PageBlocks[,1:10] # 10 Predictors (V1 to V10)
PB_class <- PageBlocks[,11] # Class labels (V11)
PB_class <- ifelse(PB_class == 1,0,1) # Inliers (class "1") = 0, Outliers (classes "2", "3", "4", "5") = 1

# Original (Non-Normalised) Data:
PB_Predictors <- PageBlocks[,1:10]
P_at_n <- rep(0, 50)
for(k in 1:50){
  LOF_Outlier <- lof(x=PB_Predictors, k = k)
  rank_LOF_Outlier <- order(x=LOF_Outlier, decreasing = TRUE)
  P_at_n[k] <- mean(PB_class[rank_LOF_Outlier[1:560]] == 1)
}
plot(1:50, P_at_n)

# Normalised Data:
PB_Predictors <- scale(PageBlocks[,1:10])
P_at_n <- rep(0, 50)
for(k in 1:50){
  LOF_Outlier <- lof(x=PB_Predictors, k = k)
  rank_LOF_Outlier <- order(x=LOF_Outlier, decreasing = TRUE)
  P_at_n[k] <- mean(PB_class[rank_LOF_Outlier[1:560]] == 1)
}
plot(1:50, P_at_n)




