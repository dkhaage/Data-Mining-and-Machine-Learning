# Global-Local Outlier Scores from Hierarchies (GLOSH)
library(dbscan)
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

MinPts <- 4 # GLOSH parameter: same as MinPts in hdbscan(), analogous to k in lof()
GLOSH_Outlier <- glosh(x=as.matrix(dat), MinPts) # GLOSH (outlier score) computation

top_n <- 20 # No. of top outliers to be displayed
rank_GLOSH_Outlier <- order(x=GLOSH_Outlier, decreasing = TRUE) # Sorting (descending)
GLOSH_Result <- data.frame(ID = rank_GLOSH_Outlier, score = GLOSH_Outlier[rank_GLOSH_Outlier])
head(GLOSH_Result, top_n)

g5 <- g0a +
  geom_point(data=dat[rank_GLOSH_Outlier[1:top_n],],mapping=aes(x=x1,y=x2),shape=19,color="red",size=2) +
  geom_text(data=dat[rank_GLOSH_Outlier[1:top_n],],
            mapping=aes(x=(x1-0.5), y=x2, label=rank_GLOSH_Outlier[1:top_n]), size=2.5)
g5

g6 <- g0a +
  geom_point(data=dat, mapping=aes(x=x1, y=x2, size = GLOSH_Outlier), shape = 1, color = "red") +
  scale_size_continuous(range = c(0.1, 15))
g6 #  GLOSH (MinPts=4): Circle radii are proportional to outlier score.

GLOSH_Outlier_Transf <- ( (GLOSH_Outlier - min(GLOSH_Outlier)) / (max(GLOSH_Outlier) - min(GLOSH_Outlier)) )^3
g7 <- g0a +
  geom_point(data=dat, mapping=aes(x=x1, y=x2, size = GLOSH_Outlier_Transf), shape = 1, color = "red") +
  scale_size_continuous(range = c(0.1, 15))
g7 
# Since the GLOSH scores fall within the interval [0,1], 
# the contrast between inliers and outliers can be improved by making the radii of the circles proportional to the GLOSH scores squared or cubed, 
# which affects much less values closer to 1

#shows the same visualisation, but now for the GLOSH scores computed using a larger value of \(MinPts\). 
# It can be seen that \(MinPts\) works as a smoothing factor, as it does for HDBSCAN*.
MinPts <- 15 # GLOSH parameter
GLOSH_Outlier <- glosh(x=as.matrix(dat), MinPts) # GLOSH (outlier score) computation
GLOSH_Outlier_Transf <- ( (GLOSH_Outlier - min(GLOSH_Outlier)) / (max(GLOSH_Outlier) - min(GLOSH_Outlier)) )^3
g9 <- g0a +
  geom_point(data=dat, mapping=aes(x=x1, y=x2, size = GLOSH_Outlier_Transf), shape = 1, color = "red") +
  scale_size_continuous(range = c(0.1, 15))
g9

top_n <- 50 # No. of top outliers to be displayed
rank_GLOSH_Outlier <- order(x=GLOSH_Outlier, decreasing = TRUE) # Sorting (descending)
g8 <- g0a +
  geom_point(data=dat[rank_GLOSH_Outlier[1:top_n],],mapping=aes(x=x1,y=x2),shape=19,color="red",size=2) +
  geom_text(data=dat[rank_GLOSH_Outlier[1:top_n],],
            mapping=aes(x=(x1-0.5), y=x2, label=c(1:top_n)), size=2.5)
g8

# PAGE BLOCKS
PageBlocks <- read.table("page-blocks.csv", header=FALSE, sep="", dec=".")

PB_Predictors <- PageBlocks[,1:10] # 10 Predictors (V1 to V10)
PB_class <- PageBlocks[,11] # Class labels (V11)
PB_class <- ifelse(PB_class == 1,0,1) # Inliers (class "1") = 0, Outliers (classes "2", "3", "4", "5") = 1

# top 50 outliers are identified by their outlier ranks ('1' means the most outlying, '2' the second most outlying, etc.), rather than their IDs.
# Original (Non-Normalised) Data:
P_at_n <- rep(0, 50)
for(MinPts in 2:50){
  GLOSH_Outlier <- glosh(x=as.matrix(PB_Predictors), MinPts)
  rank_GLOSH_Outlier <- order(x=GLOSH_Outlier, decreasing = TRUE)
  P_at_n[MinPts] <- mean(PB_class[rank_GLOSH_Outlier[1:560]] == 1)
}
plot(2:50, P_at_n[2:50])


# Normalised Data:
PB_Predictors <- scale(PageBlocks[,1:10])
P_at_n <- rep(0, 50)
for(MinPts in 2:50){
  GLOSH_Outlier <- glosh(x=as.matrix(PB_Predictors), MinPts)
  rank_GLOSH_Outlier <- order(x=GLOSH_Outlier, decreasing = TRUE)
  P_at_n[MinPts] <- mean(PB_class[rank_GLOSH_Outlier[1:560]] == 1)
}
plot(2:50, P_at_n[2:50])
















