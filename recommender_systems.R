# Recommender Systems
# install.packages('rrecsys')
library('rrecsys')

# USER BASED KNN (Collaborative Filtering)

# makes the assumption that users with similar preferences tend to rate items similarly

# The User-Based KNN method takes the k nearest neighbours of u, NNk(u), 
# as a reference subset of the users most similar to u. If the rating of user u to an item i is missing (i.e., rui=0), 
# the algorithm estimates it, in its simplest form, 
# by just computing the average rating given to item i by those neighbours of u that have rated item i

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>   Generate the user-item matrix
set.seed(0)
ratings_11 <- matrix(sample(c(0:5),size=50,replace=TRUE,prob=c(.4,.02,.02,.04,.26,.26)), nrow=10, byrow=TRUE)
ratings_12 <- matrix(sample(c(0:5),size=50,replace=TRUE,prob=c(.4,.26,.26,.04,.02,.02)), nrow=10, byrow=TRUE)
ratings_21 <- matrix(sample(c(0:5),size=50,replace=TRUE,prob=c(.4,.26,.26,.04,.02,.02)), nrow=10, byrow=TRUE)
ratings_22 <- matrix(sample(c(0:5),size=50,replace=TRUE,prob=c(.4,.02,.02,.04,.26,.26)), nrow=10, byrow=TRUE)
myratings  <- rbind(cbind(ratings_11,ratings_12),cbind(ratings_21,ratings_22))
myratings

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>    Convert the user-item matrix into an object of class dataset

# Once we have the user-item matrix, we can use the function defineData() from the package rrecsys to convert the matrix to a 
# form that can be used by the recommender algorithms in that package.
# Specifically, defineData() converts an explicit or a sparse user-item matrix representation into an object of class dataSet.
# By default it keeps ratings within the interval [minimum,maximum] = [0.5, 5], 
# but changing the default value of the attribute binary to TRUE and setting a 
# threshold positiveThreshold would result in a binary matrix where ratings above the threshold become 1 and the others become 0

library(rrecsys)
my_data_object <- defineData(myratings) #  the ratings as they are, so we don't change the default settings

# Now we can call the User-Based KNN method by setting attribute alg to "UBKNN" in function rrecsys():
r_model <- rrecsys(my_data_object, alg = "UBKNN", simFunct = "cos", neigh = 5)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>      Predict unrated items

# attribute neigh corresponds to parameter k of the method, that is, the size of the neighbourhood. 
# Different values of k will produce different models. Once a model has been obtained, 
# predictions can be made using the function predict():
pred <- predict(r_model)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>      Generate ratings rounded to the nearest integer

# In predict(), all unrated items are predicted and the entire matrix is returned with the new ratings. 
# To focus only on the predictions for the originally missing ratings, we replace the originally present ratings with NA:
pred <- predict(r_model, Round = TRUE)
estimated_ratings <- ifelse(myratings != 0, NA, pred)
print(estimated_ratings, digits = 4)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>       Extract the user and item indexes

# In recommendation, the usual goal is to recommend items to users who are likely to appreciate those items, 
# so, typically, the focus is on user-item pairs with high predicted ratings. 
# We can write a function top_m() that extracts the user and item indexes, u and i, corresponding to the m highest estimated ratings:

top_m <- function(M,m){
  top_n_indexes <- which(M>=sort(M, decreasing=TRUE)[m], arr.ind=TRUE)
  top_n_order   <- order(M[top_n_indexes], decreasing=TRUE)
  sorted_ind    <- top_n_indexes[top_n_order,]
  sorted_val    <- M[sorted_ind]
  return(data.frame(user=sorted_ind[,1], item=sorted_ind[,2], pred_rating=sorted_val)[1:m,])
} 
# all top 10 recommendations above, users u???10 have been recommended items i???5, whereas users u>10 have been recommended items i>5

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>        Prac

# Modify function top_m(M,m) to get a function top_val(M,val, that returns the user and item indexes, u and i, 
# corresponding to all the estimated ratings r^ui???val, 
# then use this function to extract all the user-item pairs with r^ui???4 (along with their estimated ratings).
top_val <- function(M,val){
  top_n_indexes <- which(M>=val, arr.ind=TRUE)
  top_n_order   <- order(M[top_n_indexes], decreasing=TRUE)
  sorted_ind    <- top_n_indexes[top_n_order,]
  sorted_val    <- M[sorted_ind]
  return(data.frame(user=sorted_ind[,1], item=sorted_ind[,2], pred_rating=sorted_val))
}

(top_ratings <- top_val(M = estimated_ratings, val = 4))

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>   Recommend the highest predicted ratings on each other

# may not need the estimated ratings for all user-item pairs, but rather the top m recommendations for each user only. 
# There are ways to speed up computations when only the top m recommendations are required, which can be more efficient
# than generating all estimated ratings and then extracting the highest ones. 
# The function recommendHPR() from the package rrecsys recommends the highest predicted ratings on each user:

recommendHPR(r_model, topN = 1) # Recommending only the highest predicted rating of each user

# Many other different recommendation strategies that are also based on the concept of nearest neighbours can be used in practical applications. For instance, the user-item matrix can be binarised by setting the
# ratings greater than a user-defined threshold equal to 1 and the other entries equal to zero. Then, each user u can be recommended the most frequent item(s) in u's neighbourhood, among those items that have not been rated by u.

# ITEM-BASED KNN

# Item-Based KNN is structurally very similar to User-Based KNN, but it works by taking items rather than users as a reference for neighbourhood computation and ratings prediction. We can think of Item-Based KNN as the 
# User-Based KNN method operating on columns rather than on rows of the user-item matrix. The assumption behind this approach is that users will prefer items that are similar to other items that they like.

# Item-Based KNN can be selected by setting alg = "IBKNN" in the function rrecsys().

# Reproduce the previous analyses for the dataset myratings, 
# now using Item-Based KNN instead of User-Based KNN. Use the same neighbourhood size, k=5.

r_model <- rrecsys(my_data_object, alg = "IBKNN", simFunct = "cos", neigh = 5)
pred <- predict(r_model, Round = TRUE)
estimated_ratings <- ifelse(myratings != 0, NA, pred)
print(estimated_ratings)
(top_ratings <- top_m(M = estimated_ratings, m = 10))


top_val <- function(M,val){
  top_n_indexes <- which(M>=val, arr.ind=TRUE)
  top_n_order   <- order(M[top_n_indexes], decreasing=TRUE)
  sorted_ind    <- top_n_indexes[top_n_order,]
  sorted_val    <- M[sorted_ind]
  return(data.frame(user=sorted_ind[,1], item=sorted_ind[,2], pred_rating=sorted_val))
}
(top_ratings <- top_val(M = estimated_ratings, val = 4))


recommendHPR(r_model, topN = 1)
