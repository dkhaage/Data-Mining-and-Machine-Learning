library(naivebayes)

Mushrooms <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data", header=FALSE, sep=",", dec=".", na.strings=c("?"))



set.seed(0)

no_observations <- dim(Mushrooms)[1]                   # total observations (8124)

no_predictors <- dim(Mushrooms)[2] - 1                 # total predictors (22)

# total variables (23) -

# dependent var is the 1st column

set.seed(0)

Feature_Set <- c()                                     # Initialise Feature Subset (empty)

Test_Error <- c()                                      # Initialise Error Subset

for(Size_Feature_Set in 1:no_predictors){              # Continue for each predictor starting from 1
  
  best_accuracy <- -Inf
  
  for(feature in 2:(no_predictors+1)){                 # Skip 1st variable (class labels)
    
    if (!(feature %in% Feature_Set)){
      
      Test_Feature_Set <- c(Feature_Set, feature)
      
      accuracy <- 0
      
      for(i in 1:10){
        
        test_index <- sample(no_observations, size=as.integer(no_observations*0.2), replace=FALSE)
        
        # 20% test
        
        training_index <- -test_index                  # 80% training
        
        candidate_variables_index <- c(1, Test_Feature_Set) # "1" is the class variable (V1),
        
        # this vector selects specific data columns
        
        NaiveBayesModel <- naive_bayes(V1 ~. ,         # . takes the available data columns   
                                       
                                       data = Mushrooms[training_index, candidate_variables_index])
        
        Pred_class <- predict(NaiveBayesModel,
                              
                              newdata = Mushrooms[test_index, candidate_variables_index])
        
        tab <- table(Pred_class, Mushrooms[test_index,"V1"])
        
        accuracy <- accuracy + sum(diag(tab))/sum(tab)
        
      }
      
      accuracy <- accuracy/10
      
      if (accuracy > best_accuracy){ # get the best feature that contributes to improve accuracy
        
        best_accuracy <- accuracy
        
        best_new_feature <- feature
        
      }
      
    }
    
  }
  
  Feature_Set <- c(Feature_Set, best_new_feature)  # list of best features in each iteration
  
  print(Feature_Set)
  
  Test_Error <- c(Test_Error, 1-best_accuracy)     # calculate the error rate in each iteration
  
}

plot(1:22,Test_Error) # finally plot the error rates

