install.packages('naivebayes')
library('naivebayes')

data(iris)
summary(iris)

set.seed(0)
no_obs <- dim(iris)[1] #  No. of observations (150)
test_index <- sample(no_obs, size = as.integer(no_obs*0.2), replace = FALSE) # 20% data records for the test
training_index <- -test_index # 80% data records for training

NaiveBayesIris <- naive_bayes(Species~., data = iris[training_index,])
NaiveBayesIris

Pred_class <- predict(NaiveBayesIris, newdata = iris[test_index,], type = "class")

Pred_class

cont_tab <- table(Pred_class, iris$Species[test_index])

cont_tab

accuracy <- sum(diag(cont_tab))/sum(cont_tab)

