# ADEC 7430: Big Data Econometrics
# R Lab #7: Support Vector Machines (pp. 359 - 368)

# Load the relevant libraries
library(e1071)
library(ROCR)
library(ISLR)

## 9.6.1 Support Vector Classifier

# Generate random observations belonging to two classes
set.seed(1)
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y==1,] <- x[y==1,] + 1

# Let's check whether the classes are linearly separable
plot(x, col = (3-y)) # they are not linearly separable

# Now we fit the support vector classifier
# Note that we must encode the response as a factor variable
dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 10, scale = F)

# Now let's plot the support vector classifier obtained
plot(svmfit, dat)

# Let's identify the seven support vectors
svmfit$index
summary(svmfit)

# What if we instead used a smaller value of the cost parameters?
svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 0.10, scale = F)
summary(svmfit)
plot(svmfit, dat)
svmfit$index
# With a smaller value of the cost parameter used, we obtain a larger number of 
# support vectors and the margin is now wider.

# Now let's compare SVMs with a linear kernel across a range of cost values
set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat, kernel = "linear", 
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out) # view the 10-fold cross-validation errors for each model
bestmod <- tune.out$best.model
summary(bestmod)

# We use the predict() functions to predict the class label on a set of
# test observations, at any given value of the cost parameter

# Let's generate a test data set
xtest <- matrix(rnorm(20*2), ncol = 2)
ytest <- sample(c(-1,1), 20, rep = TRUE)
xtest[ytest==1,] <- xtest[ytest==1,] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))

# We now use the best model obtained through CV to make predictions
ypred <- predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)

# What if used cost = 0.01
svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 0.01, scale = F)
ypred <- predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y)

# If the two classes are linearly separable, then we can find a separating hyperplane
x[y==1,] <- x[y==1,] + 0.5
plot(x, col = (y+5)/2, pch = 19)

# Now we fit the support vector classifier and plot the resulting hyperplane
dat = data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 1e5)
summary(svmfit)
plot(svmfit, dat) # not a large margin because observations (not support vectors) are close 
# to the decision boundary

# Let's using a smaller cost value
svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, dat) 


## 9.6.2 Support Vector Machine

# Let's first generate some data with a non-linear class boundary
set.seed(1)
x <- matrix(rnorm(200*2), ncol = 2)
x[1:100,] <- x[1:100,] + 2
x[101:150,] <- x[101:150,] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))

# Plot the data
plot(x, col = y)

# Randomly split the data into training and testing sets. Then fit the training data
# with a radial kernel and gamma = 1
train <- sample(200, 100)
svmfit <- svm(y ~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train,])
summary(svmfit)

# If we increase the value of cost, we can reduce the number of training errors. However,
# this creates a more irregular decision boundary prone to overfitting the data
svmfit <- svm(y ~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat[train,])

# We can perform cross-validation to select the best choice of gamma and cost for an SVM
set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat[train,], kernel = "radial", 
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                               gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)

# Assess the SVM with cost=1 and gamma=2 on the test set
svmfit <- svm(y ~., data = dat[train,], kernel = "radial", gamma = 2, cost = 1)
plot(svmfit, dat[train,])
table(true = dat[-train, "y"], pred = predict(svmfit, newx = dat[-train,]))

# Now let's view the test set predictions for this model
#table(true = dat[-train, "y"], pred = predict(tune.out$best.model,
#      newx = dat[-train,]))
# (18 + 21) / (56 + 21 + 18 + 5) = 0.39 are misclassified

# Textbook Errata
table(true = dat[-train, "y"], pred = predict(tune.out$best.model,
                                              newdata = dat[-train,]))
# (7 + 3) / (7 + 3 + 74 + 16) = 0.10 are misclassified


## 9.6.3 ROC Curves

# We first write a short function to plan an ROC curve given a vector containing
# a numerical score for each observation (pred) and a vector containing the class
# label for each observation (truth)
rocplot <- function(pred, truth, ...){
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)}


# If the fitted value exceeds zero, then the observation is assigned to one class
# if it is less than zero, than it is assigned to the other class
svmfit.opt <- svm(y ~., data = dat[train,], kernel = "radial", gamma = 2, cost = 1, decision.values = T)
fitted <- attributes(predict(svmfit.opt, dat[train,], decision.values = T))$decision.values

# We can now produce the ROC plot
par(mfrow = c(1,2))
rocplot(fitted, dat[train, "y"], main = "Training Data")

# By increasing gamma, we can produce a more flexible fit and improve accuracy
svmfit.flex <- svm(y ~., data = dat[train,], kernel = "radial", gamma = 50, cost = 1, decision.values = T)
fitted <- attributes(predict(svmfit.flex, dat[train,], decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"], add = T, col = "red")

# Plot the ROC using the Test Data
fitted <- attributes(predict(svmfit.opt, dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], main = "Test Data")
fitted <- attributes(predict(svmfit.flex, dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], add = T, col = "red")


## 9.6.4 SVM with Multiple Classes

# Perform multi-class classification using one-versus-one SVM approach
set.seed(1)
x <- rbind(x, matrix(rnorm(50*2), ncol = 2))
y <- c(y, rep(0, 50))
x[y==0, 2] <- x[y==0, 2] + 2
dat <- data.frame(x = x, y = as.factor(y))
par(mfrow = c(1, 1))
plot(x, col = (y+1))

# We now fit a SVM to the generated data
svmfit <- svm(y ~., data = dat, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit, dat)


## 9.6.5 Application to Gene Expression Data
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)

# We use a support vector approach to predict cancer subtype using gene expression measurements
# We use a linear kernel because there are a very large number of features relative to the number of obs
dat <- data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
out <- svm(y ~., data = dat, kernel = "linear", cost = 10)
summary(out)
table(out$fitted, dat$y)

# We find no training errors, as it was easy to find hyperplanes that fully separated the classes

# Now we check the performance on the test observations
dat.te <- data.frame(x = Khan$xtest, y = as.factor(Khan$ytest))
pred.te <- predict(out, newdata = dat.te)
table(pred.te, dat.te$y)  # only two test set errors with cost = 10


# R Lab #7 Solutions

# Q1
svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 10, scale = F)
plot(svmfit, dat)
summary(svmfit)

svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 0.10, scale = F)
summary(svmfit)
plot(svmfit, dat)

# Q2
svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 0.1, scale = F)
ypred <- predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y)

svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 0.01, scale = F)
ypred <- predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y)

svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 1.00, scale = F)
ypred <- predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y)

# Q3
svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 1e5)
summary(svmfit)
plot(svmfit, dat) 

svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, dat) 

# Q4
plot(x, col = y)

# Q5
svmfit <- svm(y ~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train,])

svmfit <- svm(y ~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat[train,])

# Q6
table(true = dat[-train, "y"], pred = predict(tune.out$best.model, newdata = dat[-train,]))

# Q8
svmfit <- svm(y ~., data = dat, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit, dat)