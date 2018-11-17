# Wrapper Naive Bayes

# importing necessary libraries
library(dplyr)
library(e1071)
library(naivebayes)
library(caTools)

# importing dataset
Mushrooms <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data", header=FALSE, sep=",", dec=".", na.strings=c("?"))
summary(Mushrooms)
glimpse(Mushrooms)

set.seed(0)
no_observations <- dim(Mushrooms)[1] # No. observations (8124)
no_predictors <- dim(Mushrooms)[2] - 1 # No. predictors (22) = No. variables (23) - dependent var. (1st column)
error <- 0

# ALL FEATURES
for (i in 1:10){
  test_index <- sample(no_observations, size=as.integer(no_observations*0.2), replace=FALSE) #20% data for test
  training_index <- -test_index # Remaining 80% data observations for training
  NaiveBayesModel <- naive_bayes(V1 ~., data = Mushrooms[training_index, ]) #  classifier with all variables
  Pred_class <- predict(NaiveBayesModel, newdata = Mushrooms[test_index, ]) #  prediction on test set
  tab <- table(Pred_class, Mushrooms[test_index,"V1"]) # contingency table
  accuracy <- sum(diag(tab))/sum(tab) #  calculate accuracy
  error <- error + (1 - accuracy) #  calculate error average classification error over ten different subsets
}
(error <- error/10) #  display

plot(Pred_class)
tab




# WRAPPER
for (i in 1:10){
  test_index <- sample(no_observations, size=as.integer(no_observations*0.2), replace=FALSE) #  20% data for test
  training_index <- -test_index #  Remaining 80% data observations for training
  NaiveBayesModel <- naive_bayes(V1~ V21 + V6 + V16 + V4, data = Mushrooms[training_index, ]) #  classifier with selected feactures
  Pred_class <- predict(NaiveBayesModel, newdata = Mushrooms[test_index, ]) #  prediction on test set
  tab <- table(Pred_class, Mushrooms[test_index,"V1"]) #  contingency table
  accuracy <- sum(diag(tab))/sum(tab) #  calculate accuracy
  error <- error + (1 - accuracy) #  calculate classification error
}
(error <- error/10) #  display average classification error over ten different subsets


plot(Pred_class)





















