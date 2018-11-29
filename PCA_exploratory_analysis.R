# Principal Component Analysis (PCA)
set.seed(1)
x1 <- runif(n = 100, min = 0, max = 50)
x2 <- x1 + rnorm(n = 100, mean = 0, sd = 1)
dat <- data.frame(x=x1, y=x2)
(g_PCA <- ggplot() + geom_point(data=dat, mapping=aes(x=x, y=y), shape = 19))

library(stats)
PCA <- prcomp(dat, scale = TRUE) #  calculate PC for variables
PCA$rotation 
# The variable rotation stores the resulting (rotated) coordinates system. 
# Each column represents a vector along the corresponding coordinate direction and, therefore, the columns of rotation are orthogonal.

# sdev^2, returns the respective variances and, by dividing these by the total sum of variances,
# we get the Proportion of the Variance Explained (PVE) by each component:
(PVE <- (PCA$sdev^2)/sum(PCA$sdev^2))
# he first component explains about 99.88% of the variance, whereas the second component explains the remaining 0.12%

# PAGE BLOCKS
PageBlocks <- read.table("page-blocks.csv", header=FALSE, sep="", dec=".")

PB_Predictors <- PageBlocks[,1:10] # 10 Predictors (V1 to V10)
PB_class <- PageBlocks[,11] # Class labels (V11)
PB_class <- ifelse(PB_class == 1,0,1) # Inliers (class "1") = 0, Outliers (classes "2", "3", "4", "5") = 1

# By applying PCA to the 10-dimensional space of the predictors,
# we can see that the first two components alone explain almost 60% of the variance:
PCA_PageBlocks <- prcomp(PB_Predictors, scale = TRUE)
(PVE <- (PCA_PageBlocks$sdev^2)/sum(PCA_PageBlocks$sdev^2))

# It is interesting to visualise the data in these two principal components, PC1 and PC2,
# with the ground truth outliers (according to the class labels) highlighted in blue:
PCA_2D_dat <- as.data.frame(PCA_PageBlocks$x[,1:2])
g_PCA <- ggplot() + geom_point(data=PCA_2D_dat, mapping=aes(x=PC1, y=PC2), shape = 19)
(g_ground_truth <-PCA_2D_dat[PB_class==1,], mapping=aes(x=PC1,y=PC2), shape=19, color="blue", size=2))

# PC1 and PC2, with the ground truth outliers (according to the class labels) highlighted in blue:
k <- 50
LOF_Outlier <- lof(x=PB_Predictors, k = k)
rank_LOF_Outlier <- order(x=LOF_Outlier, decreasing = TRUE)

top_n <- 560
(g_LOF_top_n <- g_PCA +
    geom_point(data=PCA_2D_dat[rank_LOF_Outlier[1:top_n],],mapping=aes(x=PC1,y=PC2),shape=19,color="red1",size=2))
# result for LOF with k=50

(g_LOF_geq_2 <- g_PCA +
    geom_point(data=PCA_2D_dat[LOF_Outlier>=2,], mapping=aes(x=PC1,y=PC2), shape=19, color="red2", size=2))
# shows in red those observations with LOF scores higher than 2

(g_LOF_geq_3 <- g_PCA +
    geom_point(data=PCA_2D_dat[LOF_Outlier>=3,], mapping=aes(x=PC1,y=PC2), shape=19, color="red3", size=2))
# Page Blocks Classification dataset (two principal components): LOF scores >= 3 highlighted in red.

# GLOSH
MinPts <- 50
GLOSH_Outlier <- glosh(x= as.matrix(PB_Predictors), k = MinPts)
rank_GLOSH_Outlier <- order(x=GLOSH_Outlier, decreasing = TRUE)

top_n <- 560
(g_GLOSH_top_n <- g_PCA +
    geom_point(data=PCA_2D_dat[rank_GLOSH_Outlier[1:top_n],],mapping=aes(x=PC1,y=PC2),shape=19,color="red1",size=1.8))

(g_GLOSH_geq_2 <- g_PCA +
    geom_point(data=PCA_2D_dat[GLOSH_Outlier>=0.9,], mapping=aes(x=PC1,y=PC2), shape=19, color="red2", size=1.8)) # Change back from matrix?

####

k <- 4
LOF_Outlier <- lof(x=PB_Predictors, k = k)
rank_LOF_Outlier <- order(x=LOF_Outlier, decreasing = TRUE)

(g_LOF_top_n <- g_PCA + geom_point(data=PCA_2D_dat[rank_LOF_Outlier[1:top_n],], mapping=aes(x=PC1,y=PC2), shape=19, color="red1", size=2))

(g_LOF_geq_2 <- g_PCA + geom_point(data=PCA_2D_dat[LOF_Outlier>=2,], mapping=aes(x=PC1,y=PC2), shape=19, color="red2", size=2))

(g_LOF_geq_3 <- g_PCA + geom_point(data=PCA_2D_dat[LOF_Outlier>=3,], mapping=aes(x=PC1,y=PC2), shape=19, color="red3", size=2))

print("DISCUSSION: Clearly, with k = 4 most of the outliers in the comet tail are missed, almost all detected outliers fall deep inside the cluster. k = 50 seems to be a much more sensible choice visually, based on the PC1 x PC2 approximation of the data. This does suggest that PCA can be used as an EDA tool, possibly helping guiding the otherwise blind parameter choice in unsupervised learning")


