# Logistic Regression Walk Through

set.seed(0)
x0_10 <- runif(10, min=0, max=60); x11_20 <- runif(10, min=40, max=100)
y0_10 <- rep(-1,10); y11_20 <- rep(+1,10)
( dat <- data.frame(x=c(x0_10,x11_20), y=c(y0_10,y11_20)) )

library('ggplot2')
class <- as.factor(dat$y)
( g <- ggplot() + geom_point(data=dat, mapping= aes(x=x, y=y, shape=class)) )



library(ggplot2)
class <- as.factor(dat$y)
( g <- ggplot() + geom_point(data=dat, mapping=aes(x=x, y=y, shape=class)) )

linear_model <- lm(y ~., data=dat)
new_dat <- data.frame(x=seq(from=0, to=100, by=0.1))
Y_hat_new_dat <- predict.lm(linear_model, newdata=new_dat)
g2 <- g + geom_point(data=data.frame(x=new_dat, y=Y_hat_new_dat), mapping=aes(x=x, y=y))
Y_hat_dat <- predict.lm(linear_model, newdata=NULL)
Y_dat <- sign(Y_hat_dat)
( g2 <- g2 + geom_point(data=dat, mapping=aes(x=x, y=y, shape=class, colour=Y_dat), show.legend=FALSE) )

LR_fun <- function(v){1/(1 + exp(-v))}
P_positive_new_dat <- LR_fun(Y_hat_new_dat)
g3 <- g + geom_point(data=data.frame(x=new_dat, y=P_positive_new_dat), mapping=aes(x=x, y=y))
P_positive_dat <- LR_fun(Y_hat_dat)
Y_dat <- sign(P_positive_dat - 0.5)
( g3 <- g3 + geom_point(data=dat, mapping=aes(x=x, y=y, shape=class, colour=Y_dat), show.legend=FALSE) )

library('stats')

dat <- cbind(dat, class)
LR_model <- glm(formula=class~x, data=dat, family=binomial)
P_positive_new_dat <- predict.glm(LR_model, newdata=new_dat, type="response")
g4 <- g + geom_point(data=data.frame(x=new_dat, y=P_positive_new_dat), mapping=aes(x=x, y=y))
P_positive_dat <- predict(LR_model, type="response")
Y_dat <- sign(P_positive_dat - 0.5)
( g4 <- g4 + geom_point(data=dat, mapping=aes(x=x, y=y, shape=class, colour=Y_dat), show.legend=FALSE) )



