# Recommender Systems Model Selection

# Looking at Mean Absolute Error(MAE), and Root Mean Squared Error(RMSE)

# The following code splits the data into two cross-validated folds using the function evalModel() from the package rrecsys, 
# then it uses the function evalPred to apply 
# the Item-Based KNN method with k=5, to model the data using the training folds and predict the ratings in the test folds 
# (only two folds have been used because the dataset is very small):


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>   Generate the user-item matrix
set.seed(0)
ratings_11 <- matrix(sample(c(0:5),size=50,replace=TRUE,prob=c(.4,.02,.02,.04,.26,.26)), nrow=10, byrow=TRUE)
ratings_12 <- matrix(sample(c(0:5),size=50,replace=TRUE,prob=c(.4,.26,.26,.04,.02,.02)), nrow=10, byrow=TRUE)
ratings_21 <- matrix(sample(c(0:5),size=50,replace=TRUE,prob=c(.4,.26,.26,.04,.02,.02)), nrow=10, byrow=TRUE)
ratings_22 <- matrix(sample(c(0:5),size=50,replace=TRUE,prob=c(.4,.02,.02,.04,.26,.26)), nrow=10, byrow=TRUE)
myratings  <- rbind(cbind(ratings_11,ratings_12),cbind(ratings_21,ratings_22))
myratings

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    Convert the user-item matrix into an object of class dataset

library(rrecsys)
my_data_object <- defineData(myratings) #  the ratings as they are, so we don't change the default settings


# IBEvaluation, contains both the MAE and RMSE values averaged over the test folds.
set.seed(0)
CV_Model <- evalModel(data = my_data_object, folds = 2) # Cross-Validation Model with 2 Folds
IBEvaluation <- evalPred(CV_Model, "IBKNN", simFunct = "cos", neigh = 5)

# Repeat the cross-validation process above for different values of k, and plot both MAE and RMSE as a function of k (from 1 to 9)
# the smaller the MAE and RMSE values are the better (more accurate) the results.

set.seed(0)
CV_Model <- evalModel(data = my_data_object, folds = 2)
MAE <- rep(0, 9)
RMSE <- rep(0, 9)
for (k in 1:9){
  IBEvaluation <- evalPred(CV_Model, "IBKNN", simFunct = "cos", neigh = k)
  MAE[k] <- IBEvaluation["Average","MAE"]
  RMSE[k] <- IBEvaluation["Average","RMSE"]
}
(plot(1:9,MAE)) # Best result is k = 7
(plot(1:9,RMSE)) # Best result is k = 7

# Repeat the previous exercise, but now with the much larger, real-world dataset mlLatest100k (Movielens Latest),
# which is available as part of the rrecsys package.

# Use 10-fold cross-validation (i.e., set the attribute folds to 10 in function evalModel())
# Vary k (i.e., attribute neigh in evalPred()) as k=1,5,10,15,20,???,45,50
# Processing may take several minutes.

data(mlLatest100k)
my_data_object <- defineData(mlLatest100k)
dataChart(my_data_object, x = "items", y = "num_of_ratings")

set.seed(0)
CV_Model <- evalModel(data = my_data_object, folds = 10)
k_max <- 50
k_set = c(1, seq(from=5,to=k_max,by=5))
no_k <- length(k_set)
MAE <- rep(0, no_k)
RMSE <- rep(0, no_k)
cont <- 0
for (k in k_set){
  IBEvaluation <- evalPred(CV_Model, "UBKNN", simFunct = "cos", neigh = k)
  cont <- cont + 1
  MAE[cont] <- IBEvaluation["Average","MAE"]
  RMSE[cont] <- IBEvaluation["Average","RMSE"]
}

plot(k_set,MAE)  # Best result is k = 50
plot(k_set,RMSE) # Best result is k = 50












