# KNN Classifier
set.seed(0)
no_obs <- dim(iris)[1] # No of observations
test_index <- sample(no_obs, size = as.integer(no_obs*0.2), replace = FALSE) #  20% data records for test
test_predictors <- iris[test_index, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
test_class <- iris[test_index, "Species"]
training_index <- -test_index # 80% data records for training
training_predictors <- iris[training_index, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
training_class <- iris[training_index, "Species"]

Pred_class <- knn(train = training_predictors, test = test_predictors, cl=training_class, k=1)
(cont_tab <- table(Pred_class, test_class))
(accuracy <- sum(diag(cont_tab))/sum(cont_tab))

# KNN Loop
set.seed(0)
accuracy <- rep(0,10)
for(i in 1:10){
  test_index <- sample(no_obs, size = as.integer(no_obs*0.2), replace = FALSE) # 20% data records for tests
  test_predictors <- iris[test_index, c("Sepal.Length","Sepal.Width","Petal.Length", "Petal.Width")]
  test_class <- iris[test_index, "Species"]
  training_index <- -test_index # 80% data records for training
  training_predictors <- iris[training_index, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
  training_class <- iris[training_index, "Species"]
  Pred_class <- knn(train=training_predictors, test=test_predictors, cl=training_class, k=1)
  cont_tab <- table(Pred_class, test_class)
  accuracy[i] <- sum(diag(cont_tab))/sum(cont_tab)
}
accuracy
mean(accuracy)

# Plot Accuracy for K one through 20
set.seed(0)
mean_accuracy <- rep(0,20)
for(k in 1:20){
  accuracy <- rep(0,10)
  for(i in 1:10){
    test_index <- sample(no_obs, size = as.integer(no_obs*0.2), replace = FALSE) # 20% data records for test
    test_predictors <- iris[test_index, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
    test_class <- iris[test_index, "Species"]
    training_index <- -test_index # 80% data records for training
    training_predictors <- iris[training_index, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
    training_class <- iris[training_index, "Species"]
    Pred_class <- knn(train=training_predictors, test=test_predictors, cl=training_class, k=1)
    cont_tab <- table(Pred_class, test_class)
    accuracy[i] <- sum(diag(cont_tab))/sum(cont_tab)
  }
  mean_accuracy[k] <- mean(accuracy)
}
plot(mean_accuracy)













