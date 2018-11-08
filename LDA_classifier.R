# LDA Classifier

library('MASS')

set.seed(0)
no_obs <- dim(iris)[1] # N0. of observations (150)
test_index <- sample(no_obs, size = as.integer(no_obs*0.20), replace = FALSE) # 20% data records for test
training_index <- -test_index # 80% data records for training
lda.fit <- lda(Species ~ ., data =iris, subset = training_index)
lda.pred <- predict(lda.fit, iris[test_index,])
lda.Pred_class <- lda.pred$class
cont_tab <- table(lda.Pred_class, iris$Species[test_index])
cont_tab
