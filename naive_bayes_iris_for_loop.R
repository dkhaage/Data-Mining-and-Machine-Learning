accuracy <- rep(0,10)
no_obs <- dim(iris)[1] # No. of observations (150)
set.seed(0)
for (i in 1:10){
  test_index <- sample(no_obs, size = as.integer(no_obs*0.2), replace = FALSE) #  20% of data records for test
  training_index <- -test_index # 80% data records for training
  NaiveBayesIris <- naive_bayes(Species ~., data = iris[training_index,])
  Pred_class <- predict(NaiveBayesIris, iris[test_index,], type = 'class')
  (cont_tab <- table(Pred_class, iris$Species[test_index]))
  (accuracy[i] <- sum(diag(cont_tab))/sum(cont_tab))
} 

accuracy
mean(accuracy)
sd(accuracy)
