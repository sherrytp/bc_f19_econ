# ADEC 7430: Big Data Econometrics
# R Lab #6: Tree-based Methods (pp. 324 - 331)

### 8.3 Lab: Decision Trees

# Load the relevant libraries
library(ISLR)
library(tree)
library(MASS)
library(randomForest)
library(gbm)

## 8.3.1 Fitting Classification Trees

# Load the Carseats data set
attach(Carseats)

# Re-code the "Sales" as a binary variable
High <- ifelse(Sales <= 8, "No", "Yes")

# We use the data.frame() function to merge High with the rest of the data
Carseats <- data.frame(Carseats, High)

# Fit a classification tree in order to predict High using all variables but Sales
tree.carseats <- tree(High ~ . - Sales, data = Carseats)
summary(tree.carseats)  # Training Error Rate = 9%
plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.carseats

# We next split the observations into a training set and a test set in order
# to properly estimate the test error. Thus, we build the tree using the training set
# and evaluate its performance on the test set
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
head(train)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]
tree.carseats <- tree(High ~ .-Sales, data = Carseats, subset = train)
tree.pred <- predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
mean(tree.pred == High.test) # Correct Prediction Rate = 71.5%

# Now we consider whether pruning the tree might lead to improved results
# We perform cross-validation to determine the optimal level of tree complexity
# cost complexity pruning is used to select a sequence of trees for consideration
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats

# Plot the error rate as a function of both size and k
par(mfrow = c(2, 1))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

# We now prune the tree to obtain the nine-node tree
prune.carseats <- prune.misclass(tree.carseats, best = 9)
summary(prune.carseats)
prune.carseats
par(mfrow = c(1, 1))
plot(prune.carseats)
text(prune.carseats, pretty = 0)

# How well does this pruned tree perform on the test data set?
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
mean(tree.pred == High.test) # Correct Prediction Rate = 77%

# If we increase the value of best, we obtain a larger pruned tree with lower classification accuracy
prune.carseats <- prune.misclass(tree.carseats, best = 15)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
mean(tree.pred == High.test) # Correct Prediction Rate = 74%

## 8.3.2 Fitting Regression Trees

# Create a training set from the Boston data set
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
names(Boston)

# Fit a regression tree to the training data
tree.boston <- tree(medv ~ ., data = Boston, subset = train)
summary(tree.boston)

# Plot the regression tree
plot(tree.boston)
text(tree.boston, pretty = 0)

# See whether pruning the tree improves performance
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")
# The most complex tree is selected by CV

# Prune the tree
prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

# In keeping with the CV results, we use the unpruned tree to make predictions on the test set
yhat <- predict(tree.boston, newdata = Boston[-train,])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0, 1)
mean((yhat - boston.test)^2) # test MSE = 25.05, RMSE = 5.005
# This indicates that this model leads to test predictions that are
# within around $5,005 of the true median home value for the suburb

## 8.3.3 Bagging and Random Forests

# Recall that bagging is simply a special case of a RF with m = p
# First we perform bagging
set.seed(1)
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13, importance = TRUE)
bag.boston

# How well does this bagged model perform on the test set?
yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
boston.test <- Boston[-train, "medv"]
plot(yhat.bag, boston.test)
abline(0, 1)
mean((yhat.bag - boston.test)^2) # test MSE = 13.47

# We can change the number of trees grown
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13, ntree = 25)
yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
mean((yhat.bag - boston.test)^2) # test MSE = 13.43

# Growing a random forest proceeds exactly in the same way, except that we use
# a smaller value of the mtry argument
set.seed(1)
rf.boston <- randomForest(medv ~., data = Boston, subset = train, mtry = 6, importance = T)
yhat.rf <- predict(rf.boston, newdata = Boston[-train,])
mean((yhat.rf - boston.test)^2) # test MSE = 11.48 (an improvement over bagging)

# View and plot the importance of each variable
importance(rf.boston)
varImpPlot(rf.boston)
# The wealth level of the community (lstat) and the house size (rm) are by far 
# the two most important variables.

## 8.3.4 Boosting

# We fit boosted regression trees to the Boston data set
set.seed(1)
boost.boston <- gbm(medv ~., data = Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(boost.boston)

# Produce "partial dependence plots" for the two most important variables
par(mfrow = c(2, 1))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")

# Use the boosted model to predict "medv" on the test set
yhat.boost <- predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2)  # test MSE = 11.8

# We can perform boosting with a different value of the shrinkage parameters lambda
boost.boston <- gbm(medv ~., data = Boston[train,], distribution = "gaussian", n.trees = 5000, 
                    interaction.depth = 4, shrinkage = 0.2, verbose = FALSE)
yhat.boost <- predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2)  # test MSE = 11.511


# R Lab #6 Solutions

# Q1
# Income < 57 10  12.220 No ( 0.70000 0.30000 )  
.70 * 10  # 7 observations have response values of "No"
.30 * 10  # 3 observations have response values of "Yes"

# CompPrice < 110.5 5   0.000 No ( 1.00000 0.00000 ) *
1.0 * 5  # 5 observations have the response value of "No"

# Q2
prune.carseats <- prune.misclass(tree.carseats, best = 9)
summary(prune.carseats)
prune.carseats
par(mfrow = c(1, 1))
plot(prune.carseats)
text(prune.carseats, pretty = 0)

# Q3
# If we decrease the value of best to 13, we obtain the same classification accuracy as best = 9
prune.carseats <- prune.misclass(tree.carseats, best = 13)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
mean(tree.pred == High.test) # Correct Prediction Rate = 77%

# Q4
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
names(Boston)
tree.boston <- tree(medv ~ ., data = Boston, subset = train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty = 0)

# Q5
plot(yhat.bag,boston.test)
plot(yhat,boston.test)

# Q6
set.seed(1)
rf2.boston <- randomForest(medv ~., data = Boston, subset = train, importance = T)
yhat.rf2 <- predict(rf2.boston, newdata = Boston[-train,])
mean((yhat.rf2 - boston.test)^2) # test MSE = 11.72

# Q7
plot(boost.boston, i = "dis")

# Q8
set.seed(1)
boost.boston2 <- gbm(medv ~., data = Boston[train,], distribution = "gaussian", n.trees = 5000, 
                     interaction.depth = 4, shrinkage = 0.01)
yhat.boost2 <- predict(boost.boston2, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost2 - boston.test)^2)  # test MSE = 10.2

# Q9
set.seed(1)
boost.boston3 <- gbm(medv ~., data = Boston[train,], distribution = "gaussian", n.trees = 5000, 
                     interaction.depth = 3, shrinkage = 0.01)
yhat.boost3 <- predict(boost.boston3, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost3 - boston.test)^2)  # test MSE = 10.5
