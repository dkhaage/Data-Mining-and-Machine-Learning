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


# YearsEmployed, CreditScore, and AccountBalance


numeric_predictors2 <- c("YearsEmployed","CreditScore","AccountBalance")

set.seed(0)
no_obs <- dim(Data)[1]
test_index <- sample(no_obs, size = as.integer(no_obs*0.2), replace = FALSE) # 20% data records for test
test_predictors <- Data[test_index, numeric_predictors2]
test_class_labels <- Data[test_index, "Approved"]
training_index <- -test_index # 80% data records for training
training_dat <- Data[training_index, c(numeric_predictors2, "Approved")]

# Classifier2
LR_CRX <- glm(formula=Approved~., data=training_dat, family=binomial)
P_positive_test <- predict.glm(LR_CRX, newdata=test_predictors, type="response")
Pred_class <- sign(P_positive_test - 0.5)
cont_tab <- table(Pred_class, test_class_labels)
accuracy <- sum(diag(cont_tab))/sum(cont_tab)

accuracy
summary(LR_CRX)




