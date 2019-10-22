graphics.off()
rm(list = ls())
setwd("~/Desktop/boston college/Big data/Week07/group project")
charity <- read.csv(file.choose())

library(psych)
describe(charity)
colnames(charity)[sapply(charity,anyNA)]
charity.train <- charity[charity$part=="train",]
library(ggplot2)
library(tidyr)
library(purrr)
charity.train[,-1] %>%
  keep(is.numeric) %>%
  gather()  %>%
  ggplot(aes(value)) + 
  facet_wrap(~ key, scales = "free") + 
  geom_density()

#data processing
charity.t <- charity
charity.t$avhv <- log(charity.t$avhv)

data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,2:21]
y.train.donr <- data.train[,22] 
y.train.damt <- data.train[y.train.donr==1,23] 
n.train.donr <- length(y.train.donr)
n.train.damt <- length(y.train.damt) 

data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:21]
y.valid.donr <- data.valid[,22] 
y.valid.damt <- data.valid[y.valid.donr==1,23] 
n.valid.donr <- length(y.valid.donr) 
n.valid.damt <- length(y.valid.damt) 

data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] 
x.test <- data.test[,2:21]


# standardize to have zero mean and unit sd

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)

# performing the standardization at this step

x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) 
apply(x.train.std, 2, mean) 
apply(x.train.std, 2, sd) 
data.train.std.donr <- data.frame(x.train.std, donr=y.train.donr) 
data.train.std.damt <- data.frame(x.train.std[y.train.donr==1,], damt=y.train.damt) 

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) 
data.valid.std.donr <- data.frame(x.valid.std, donr=y.valid.donr) 
data.valid.std.damt <- data.frame(x.valid.std[y.valid.donr==1,], damt=y.valid.damt) 

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) 
data.test.std <- data.frame(x.test.std)

## Classification Model
# LDA model
library(MASS)

model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.donr) # include additional terms on the fly using I()

# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model

post.valid.lda1 <- predict(model.lda1, data.valid.std.donr)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda1 <- cumsum(14.5*y.valid.donr[order(post.valid.lda1, decreasing=T)]-2)
plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid.lda <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid.lda, max(profit.lda1)) # report number of mailings and maximum profit
# 1329.0 11624.5

cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid.lda+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, y.valid.donr)

# QDA model
model.qda <- qda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                   avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                 data.train.std.donr)
post.valid.qda <- predict(model.qda, data.valid.std.donr)$posterior[,2]

profit.qda <- cumsum(14.5*y.valid.donr[order(post.valid.qda,decreasing = T)]-2)
plot(profit.qda)
n.mail.valid.qda <- which.max(profit.qda)
c(n.mail.valid.qda,max(profit.qda))
cutoff.qda <- sort(post.valid.qda,decreasing = T)[n.mail.valid.qda+1]
chat.valid.qda <- ifelse(post.valid.qda>cutoff.qda, 1, 0)
table(chat.valid.qda, y.valid.donr)

# Log model

model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.donr, family=binomial("logit"))

post.valid.log1 <- predict(model.log1, data.valid.std.donr, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.log1 <- cumsum(14.5*y.valid.donr[order(post.valid.log1, decreasing=T)]-2)
plot(profit.log1) # see how profits change as more mailings are made
n.mail.valid.log <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid.log, max(profit.log1)) # report number of mailings and maximum profit
# 1291.0 11642.5

cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid.log+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log1, y.valid.donr)

# GAM model
library(gam)
model.gam <- gam(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                   avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                 data.train.std.donr, family="gaussian")
post.valid.gam <- predict(model.gam, data.valid.std.donr)
profit.gam <- cumsum(14.5*y.valid.donr[order(post.valid.gam,decreasing = T)]-2)
plot(profit.gam)
n.mail.valid.gam <- which.max(profit.gam)
c(n.mail.valid.gam,max(profit.gam))
cutoff.gam <- sort(post.valid.gam, decreasing = T)[n.mail.valid.gam+1]
chat.valid.gam <- ifelse(post.valid.gam>cutoff.gam,1,0)
table(chat.valid.gam,y.valid.donr)

# KNN model
library(class)
data.train.std.donr.label <- data.train.std.donr[,1]
post.valid.knn <- knn(train = data.train.std.donr,test = data.valid.std.donr, cl = data.train.std.donr.label, k = 45)
table(post.valid.knn,y.valid.donr)

# randomforest model
library(randomForest)
model.rdmf <-randomForest(as.factor(donr)~.,data.train.std.donr,mtry=16,ntree=500)
post.valid.rdmf <-predict(model.rdmf,data.valid.std.donr)
table(post.valid.rdmf, y.valid.donr)

# Decision tree model
library(rpart)
model.dt <- rpart(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.donr, method = "class")
post.valid.dt <- predict(model.dt,data.valid.std.donr)[,2]
profit.dt<- cumsum(14.5*y.valid.donr[order(post.valid.dt,decreasing=T)]-2)
plot(profit.dt)
n.mail.valid.dt<- which.max(profit.dt)
c(n.mail.valid.dt, max(profit.dt))
cutoff.dt<- sort(post.valid.dt,decreasing = T)[n.mail.valid.dt+1]
chat.valid.dt<- ifelse(post.valid.dt>cutoff.dt,1,0)
table(chat.valid.dt,y.valid.donr)

# select model.log1 since it has maximum profit in the validation sample

post.test <- predict(model.log1, data.test.std, type="response") # post probs for test data

# Oversampling adjustment for calculating number of mailings for test set

n.mail.valid <- which.max(profit.log1)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid.log/n.valid.donr)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.donr-n.mail.valid.log)/n.valid.donr)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)


#2 Prediction modelling
  #2.1 Least square regression
model.ols <- lm(damt ~  reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.damt)
summary(model.ols)
coef.ols <- data.frame(summary(model.ols)$coef[summary(model.ols)$coef[,4] <= .01, 4])
library(car)
vif(model.ols)
pred.valid.ols <- predict(model.ols, newdata = data.valid.std.damt)
pred.ols.mse <- mean((y.valid.damt - pred.valid.ols)^2)


  #2.2.1 Best Subset with BIC
library(leaps)
regfit.full <- regsubsets(damt ~  reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                         avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                       data = data.train.std.damt, nvmax=19)
summary(regfit.full)
par("mar")
par(mar=c(1,1,1,1))
par(mfrow = c(1,1))
plot(regfit.full, scale = "bic")
reg.summary <- regfit.full
plot(reg.summary$rss, xlab = "Number of Predictors", ylab = "RSS")
reg.summary = summary(regfit.full)
names(regfit.full)
reg.summary$bic
par("mar")
par(mar=c(2,2,2,2))
par(mfrow = c(1,2))
plot(regfit.full, scale = "bic")
plot(reg.summary$bic, xlab = "Number of Predictors", ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(10, reg.summary$bic[10], col = "red", cex = 2, pch = 20) 
coef(regfit.full, 10)

predict.regsubsets <- function(object, newdata, id,...){
  form <- as.formula(object$call[[2]]) 
  mat <- model.matrix(form, newdata) 
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars]%*%coefi 
}
pred.valid.bic <- predict(regfit.full, newdata = data.valid.std.damt,id = 10)
pred.bic.mse <- mean((y.valid.damt - pred.valid.bic)^2) 

  #2.2.2 Best Subset with Adjusted R^2
par("mar")
par(mar=c(2,2,2,2))
par(mfrow = c(1,2))
plot(regfit.full, scale = "adjr2")
plot(reg.summary$adjr2, xlab = "Number of Predictors", ylab = "adj r2", type = "l")
which.max(reg.summary$adjr2)
points(16, reg.summary$adjr2[16], col = "red", cex = 2, pch = 20) 
coef(regfit.full, 16)
pred.valid.adjr2 <- predict(regfit.full, newdata = data.valid.std.damt,id = 16)
pred.adjr2.mse <- mean((y.valid.damt - pred.valid.adjr2)^2) 
 
  #2.2.3 Best Subset with Cp
par("mar")
par(mar=c(1,1,1,1))
par(mfrow = c(1,2))
plot(regfit.full, scale = "Cp")
plot(reg.summary$cp, xlab = "Number of Predictors", ylab = "cp", type = "l")
which.min(reg.summary$cp)
points(13, reg.summary$cp[13], col = "red", cex = 2, pch = 20) 
coef(regfit.full, 13)
pred.valid.cp <- predict(regfit.full, newdata = data.valid.std.damt,id = 13)
pred.cp.mse <- mean((y.valid.damt - pred.valid.cp)^2) 






  # 2.3 Best Subsets using validation set

regfit.best = regsubsets(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                          avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
                        data=data.train.std.damt,nvmax=20)
test.mat = model.matrix(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                        avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
                      data=data.valid.std.damt)
val.errors=rep(NA,20)
for(i in 1:20){
  coefi = coef(regfit.best,id=i)
  pred.model.val = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean((data.valid.std.damt$damt-pred.model.val)^2)
}
par(mfrow=c(1,1))
plot(val.errors, xlab = "Number of Predictors", type = "l")
which.min(val.errors)
points(10, val.errors[10], col = "red", cex = 2, pch = 20) 
coef(regfit.best,10)
pred.valid.val <- predict(regfit.best, newdata = data.valid.std.damt,id = 10)
pred.val.mse <- mean((y.valid.damt - pred.valid.val)^2) 

  # 2.4 Best Subsets using 5-fold cross-validation
set.seed(1)
k = 5
folds <- sample(1:k, nrow(data.train.std.damt), replace = TRUE) 
table(folds)
cv.errors <- matrix(NA, k, 19, dimnames=list(NULL, paste(1:19))) 
for(j in 1:k){
  best.fit = regsubsets(damt ~ reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                          avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,data=data.train.std.damt[folds!=j,],nvmax=19) 
  for(i in 1:19){
    pred = predict(best.fit, newdata = data.train.std.damt[folds==j,],id=i)
    cv.errors[j,i]=mean((data.train.std.damt$damt[folds==j]-pred)^2) 
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
which.min(mean.cv.errors)
par(mfrow = c(1,1))
plot(mean.cv.errors,xlab = "Number of Predictors", ylab = "CV")
points(13,mean.cv.errors[13],col = "red", cex = 2, pch = 20)
reg.cv.best <- regsubsets ( damt~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                              avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
                            data=data.train.std.damt,nvmax=20)
coef(reg.cv.best, 13)
pred.valid.cv <- predict(reg.cv.best, newdata = data.valid.std.damt, id = 13)
pred.cv.mean <- mean((y.valid.damt - pred.valid.cv)^2)

  # 2.5 PCR & PLS
install.packages("pls")
library(pls)
set.seed (1)
model.pcr <- pcr(damt~reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                   avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data = data.train.std.damt, scale = TRUE, validation = "CV")
summary(model.pcr)
par(mfrow = c(1,3))
validationplot(model.pcr, val.type = "RMSEP")
validationplot(model.pcr, val.type = "MSEP")
validationplot(model.pcr, val.type = "R2")
pred.valid.pcr <- predict(model.pcr, newdata = data.valid.std.damt, ncom = 20)
pred.pcr.mse <- mean((y.valid.damt - pred.pcr)^2)


   #2.6 Bagging and Random Forest
install.packages("randomForest")
library(randomForest)
set.seed(1)
bag.train=randomForest(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                         avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
                       data=data.train.std.damt,mtry=5,ntree=500)
bag.train
yhat.bag = predict(bag.train,newdata=data.valid.std.damt)
pred.bag.mse <- mean((yhat.bag-y.valid.damt)^2)
set.seed(1)
rf.train=randomForest(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                        avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
                      data=data.train.std.damt,mtry=20,importance=TRUE)
rf.train
yhat.rf = predict(rf.train,newdata=data.valid.std.damt)
set.seed(1)
rf.train=randomForest(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                        avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
                      data=data.train.std.damt,mtry=7,importance=TRUE)
rf.train

set.seed(1)
rf.train=randomForest(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                        avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
                      data=data.train.std.damt,mtry=6,importance=TRUE)
rf.train
yhat.rf = predict(rf.train,newdata=data.valid.std.damt)
pred.rf.mse <- mean((yhat.rf-y.valid.damt)^2)
importance(rf.train)
par(mfrow = c(1,1))
varImpPlot(rf.train)

   #2.6 shrinkage parameter
install.packages("gbm")
library(gbm)
set.seed(1)
boost.train=gbm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
                data=data.train.std.damt,distribution="gaussian",n.trees=10000,interaction.depth=4, cv.folds = 5)
boost.train

n.trees = seq(from=100 ,to=10000, by=100)
predmatrix <- predict(boost.train, newdata = data.valid.std.damt,n.trees = n.trees)
dim(predmatrix)
predict(boost.train, newdata = data.valid.std.damt,n.trees = n.trees)
test.error<-with(data.valid.std.damt,apply( (y.valid.damt - predmatrix)^2,2,mean))
par(mfrow = c(1,1))
plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")
abline(h = min(test.error),col="red")

boost.train=gbm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
                data=data.train.std.damt,distribution="gaussian",n.trees=1000,interaction.depth=4, cv.folds = 5)
boost.train
yhat.boost=predict(boost.train,newdata=data.valid.std.damt,n.trees=1000)
mean((yhat.boost-y.valid.damt)^2)

boost.train=gbm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
                data=data.train.std.damt,distribution="gaussian",n.trees=1000,interaction.depth=4, shrinkage = 0.01, cv.folds = 5)
boost.train
summary(boost.train)
yhat.boost=predict(boost.train,newdata=data.valid.std.damt,n.trees=1000)
mean((yhat.boost-y.valid.damt)^2)

boost.train=gbm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
                data=data.train.std.damt,distribution="gaussian",n.trees=1000,interaction.depth=4, shrinkage = 0.001, cv.folds = 5)
boost.train
yhat.boost=predict(boost.train,newdata=data.valid.std.damt,n.trees=1000)
mean((yhat.boost-y.valid.damt)^2)


  #2.7 Ridge Regression with cross-validation
library(glmnet)
grid <- 10^seq(10, -2, length = 100)
x <- model.matrix(damt ~.-damt,
                  data=data.train.std.damt)
y<- data.train.std.damt$damt
xv <- model.matrix(damt ~.-damt,
                   data=data.valid.std.damt)
yv <- data.valid.std.damt$damt
model.ridge <- glmnet(x,y, alpha = 0, lambda = grid)
par(mfrow = c(1,2))
plot(model.ridge, xvar = "lambda", label = TRUE)
dim(coef(model.ridge))
set.seed(1)
cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge)
bestlam = cv.ridge$lambda.min
log(bestlam)
pred.valid.ridge =predict(model.ridge,s=bestlam,newx=xv)
summary(pred.valid.ridge)
pred.ridge.mse <- mean((pred.valid.ridge-y.valid.damt)^2)
predict(cv.ridge,type="coefficients",s=bestlam)[1:22,]
coef(cv.ridge)

  #2.8 Lasso Regression with cross-validation
model.lasso = glmnet(x,y,alpha=1,lambda=grid)
plot(model.lasso)
set.seed(1)
cv.lasso=cv.glmnet(x,y,alpha=1)
plot(cv.lasso)
bestlam =  cv.lasso$lambda.min
log(bestlam)
pred.valid.lasso =predict(model.lasso,s=bestlam,newx=xv)
summary(pred.valid.lasso)
pred.lasso.mse <- mean((pred.valid.lasso-y.valid.damt)^2)
predict(cv.lasso,type="coefficients",s=bestlam)[1:22,]
coef(cv.lasso)

  ##result
library(gbm)
final.pred.model =gbm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
                data=data.train.std.damt,distribution="gaussian",n.trees=1000,interaction.depth=4, shrinkage = 0.01, cv.folds = 5)
yhat.test <- predict(final.pred.model, newdata = data.test.std, n.trees = 1000)
length(yhat.test)
yhat.test[1:10]




