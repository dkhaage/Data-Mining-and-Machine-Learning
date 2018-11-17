# Classification Assessment

set.seed(0)
x0_10 <- runif(10, min=0, max=60); x11_20 <- runif(10, min=40, max=100)
y0_10 <- rep(-1,10); y11_20 <- rep(+1,10)
( dat <- data.frame(x=c(x0_10,x11_20), y=c(y0_10,y11_20)) )

# K-Fold Cross Validataion 
library(class)

predictors <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
class_labels <- iris[, "Species"]
Pred_class <- knn.cv(train = predictors, cl =class_labels, k=1)
(cont_tab <- table(Pred_class, class_labels))
(accuracy <- sum(diag(cont_tab))/sum(cont_tab))

# Vary paramater k for 1:20
accuracy <- numeric(20)
predictors <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
class_labels <- iris[,"Species"]
for(k in 1:20){
  Pred_class <- knn.cv(train = predictors, cl=class_labels, k=k)
  cont_tab <- table(Pred_class, class_labels)
  accuracy[k] <- sum(diag(cont_tab))/sum(cont_tab)
}
plot(accuracy)

# Area Under the Reciever Operating Characteristic Curve (AUC-ROC)

# Logistic Regression
Data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",
                   header = FALSE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "",
                   na.strings =  "?")

names(Data) <- c("Gender", "Age", "MonthlyExpenses", "MaritalStatus", "HomeStatus", "Occupation",
                 "BankingInstitution", "YearsEmployed", "NoPriorDefault", "Employed", "CreditScore",
                 "DriversLicense", "AccountType", "MonthlyIncome", "AccountBalance", "Approved")

Data <- na.omit(Data) # Remove rows with NA values

numeric_predictors <- c("Age","MonthlyExpenses","YearsEmployed","CreditScore","MonthlyIncome","AccountBalance")

set.seed(0)
no_obs <- dim(Data)[1]
test_index <- sample(no_obs, size = as.integer(no_obs*0.2), replace = FALSE) # 20% data records for test
test_predictors <- Data[test_index, numeric_predictors]
test_class_labels <- Data[test_index, "Approved"]
training_index <- -test_index # 80% data records for training
training_dat <- Data[training_index, c(numeric_predictors, "Approved")]

# Classifier
LR_CRX <- glm(formula=Approved~., data=training_dat, family=binomial)
P_positive_test <- predict.glm(LR_CRX, newdata=test_predictors, type="response")
Pred_class <- sign(P_positive_test - 0.5)
cont_tab <- table(Pred_class, test_class_labels)
accuracy <- sum(diag(cont_tab))/sum(cont_tab)

accuracy
summary(LR_CRX)

# ROC
library(ROCR)
# Auxiliary Function
rocplot <- function(pred, truth){
  predobj <- prediction(pred, truth)
  ROC     <- performance(predobj, "tpr", "fpr")
  plot(ROC)   # Plot the ROC Curve
  auc     <- performance(predobj, measure = "auc")
  auc     <- auc@y.values[[1]]
  return(auc) # Return the Area Under the Curve ROC
}
AUC <- rocplot(pred=P_positive_test, truth=test_class_labels)
AUC

# KNN - ROC
predictors <- Data[, c("YearsEmployed", "CreditScore", "AccountBalance")]
predictors <- as.matrix(scale(predictors)) # Variable scaling required by KNN
class_labels <- Data[, "Approved"]
for(k in 1:20){
  Pred_class <- knn.cv(train=predictors, cl=class_labels, k=k, prob=TRUE)
  Pred_prob <- attr(Pred_class, "prob")
  Pred_prob <- ifelse(Pred_class=='+', Pred_prob, 1 - Pred_prob) # Make sure probabilities are for class "+"
  AUC[k] <- rocplot(pred=Pred_prob, truth=class_labels)}
AUC
plot(AUC)


