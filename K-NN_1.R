# K-NN

# Remove Undesired Predictors
new_iris_predictors <- iris[, c("Sepal.Width", "Petal.Width")]
new_iris_class <- iris[, "Species"]

# find target observation in training set
( target <- which((iris$Sepal.Width==2.5)&(iris$Petal.Width==1.5)) )

# Remove target from training
new_iris_test <- new_iris_predictors[target,]
new_iris_predictors <- new_iris_predictors[-target,]
new_iris_class <- new_iris_class[-target]

library('class')

(knn(train = new_iris_predictors, test = new_iris_test, cl = new_iris_class, k = 1, prob = TRUE))

(knn(train = new_iris_predictors, test = new_iris_test, cl = new_iris_class, k = 3, prob = TRUE))

(knn(train = new_iris_predictors, test = new_iris_test, cl = new_iris_class, k = 5, prob = TRUE))

(knn(train = new_iris_predictors, test = new_iris_test, cl = new_iris_class, k = 7, prob = TRUE))

# extract composed result
pred_value <- knn(train = new_iris_predictors, test = new_iris_test, cl = new_iris_class, k = 7, prob = TRUE)
attr(pred_value, "prob")
