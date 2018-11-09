# Naive Bayes Mushroom Set

Mushrooms <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data", header=FALSE, sep=",", dec = ".", na.strings = c("?"))
summary(Mushrooms)

# Random Split
set.seed(0)
no_observations <- dim(Mushrooms)[1] # No. observations (8124)
no_predictors <- dim(Mushrooms)[2] # No. predictors (22) = No.variables (23) - dependent var.(1st column)

test_index <- sample(no_observations, size =as.integer(no_observations*0.2), replace = FALSE) # 20% data for test
training_index <- -test_index # Remaining 80% data observations for training

# Naive Bayes
library(naivebayes)
NaiveBayesModel <- naive_bayes(V1 ~ ., data = Mushrooms[training_index, ])

Pred_class <- predict(NaiveBayesModel, newdata = Mushrooms[test_index, ])
tab <- table(Pred_class, Mushrooms[test_index,"V1"])
accuracy <- sum(diag(tab))/sum(tab)
(error <- 1-accuracy)
accuracy

# Naive Bayes Mean Accuracy
error <- 0

for (i in 1:10){
  test_index <- sample(no_observations, size =as.integer(no_observations*0.2), replace = FALSE) # 20% data for test
  training_index <- -test_index # Remaining 80% data observations for training
  NaiveBayesModel <- naive_bayes(V1 ~ ., data = Mushrooms[training_index, ])
  Pred_class <- predict(NaiveBayesModel, newdata = Mushrooms[test_index, ])
  tab <- table(Pred_class, Mushrooms[test_index,"V1"])
  accuracy <- sum(diag(tab))/sum(tab)
  error <- error + (1 - accuracy)
  accuracy
}

(error <- error/10)
