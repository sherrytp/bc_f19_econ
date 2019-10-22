# ADEC 7430: Big Data Econometrics
# R Lab #5: Classification Models (pp. 154 - 167)

# 4.6.1 The Stock Market Data

# Load the relevant libraries
library(ISLR)

# We begin by examining some numerical and graphical summaries of the Stock Market Data
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)    # scatter plot matrix of data variables
cor(Smarket[,-9]) # correlation matrix of quantitative data variables

# As evident, the correlations between the lag variables and today's returns are nearly 0
# This indicates that there is little correlation b/t today's returns and the previous days' returns
attach(Smarket)
plot(Volume)  # the average number of shares traded is increasing over time (correlation b/t volume and year)

# 4.6.2 Logistic Regression

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fit)
coef(glm.fit) # get the coefficients of the fitted model
summary(glm.fit)$coef[,4] # get the p-values for the coefficients

# We now predict the probability that the market will go up, given the values of the predictors
glm.probs <- predict(glm.fit, type = "response")  # probabilities computed for the training data
glm.probs[1:10]
contrasts(Direction)

# We must convert these predicted probabilities into class labels, Up or Down
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"

# Given these predictions, produce a confusion matrix for classification/misclassification
table(glm.pred, Direction)
(507 + 145)/1250
mean(glm.pred == Direction)
# The diagonal elements of he confusion matrix indicate correct predictions
# The logistic regression correctly predicted the movement of the market 52.2% of the time
# Training Error Rate = 100 - 52.2 = 47.8%

# We need to create a training and test data set
train <- (Year < 2005)
Smarket.2005 <- Smarket[!train,]  # sub-matrix of stock market data with observations from 2005
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

# Fit a logistic regression model using only the subset of observations that correspond to data before 2005
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, 
               subset = train)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)  # computes the test error rate = 52%

# Next, we re-fit the logistic regression model using Lag1 and Lag2 only, as these had lowest p-values
glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Direction.2005)   # determines the confusion matrix
mean(glm.pred == Direction.2005)
106 / (106 + 76)

# Suppose we want to predict Direction on a day when Lag1 = 1.2 and Lag2 = 1.1, and on 
# a day when Lag1 = 1.5 and Lag2 = -0.8
predict(glm.fit, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type = "response")

# 4.6.3 Linear Discriminant Analysis

library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit   # provides the prior probabilities, group means and coeff. of linear discriminants
plot(lda.fit) # produces a plot of the linear discriminants

# Use the predict() function to get predictions, posterior probabilities, and linear discriminants
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class <- lda.pred$class
lda.post <- lda.pred$posterior
lda.class
lda.post

table(lda.class, Direction.2005)  # determines the confusion matrix
mean(lda.class == Direction.2005) # determines the test set error

# Apply a 50% threshold to the posterior probabilities to recreate the predictions
sum(lda.pred$posterior[,1] >= .5)
sum(lda.pred$posterior[,1] < .5)

# The posterior probability output by the model corresponds to the probability that the market will decrease
lda.pred$posterior[1:20,1]
lda.class[1:20]

# Here, we use a posterior probability treshold other than 50% to make predictions
sum(lda.pred$posterior[,1] > .9)

# 4.6.4 Quadratic Discriminant Analysis

qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit

qda.class <- predict(qda.fit, Smarket.2005)$class # make predictions using the test set
table(qda.class, Direction.2005)  # determines the confusion matrix
mean(qda.class==Direction.2005)   # determines the test set error
# This suggests that the quadratic form assumed by QDA may capture the true relationship
# more accurately than the linear forms by LDA and Logistic Regression

# 4.6.5 K-Nearest Neighbors

library(class)
train.X <- cbind(Lag1, Lag2)[train,]
test.X <- cbind(Lag1, Lag2)[!train,]
train.Direction <- Direction[train]

# If several observations are tied as nearest neighbors, we randomly break the tie
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005) # determines the confusion matrix
(83+43)/252 # only 50% of the observations are correctly predicted

# 4.6.6 An Application to Caravan Insurance Data

dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822  # only 6% of people purchased caravan insurance

# For KNN, the scale of the variables matters, so we standardize the data (mean=0, sd=1)
standardized.X <- scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

# We now split the standardized observations into a test set (first 1000 obs) and a training set (remaining)
test <- 1:1000
train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]

# Now we fit a KNN model on the training data using K = 1 and evaluate performance on the test data.
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)  # KNN error rate on the test set = 12%
mean(test.Y != "No")  # predic "No" regardless of the values of predictors = 6% error

# Suppose the overall error rate is not of interest, but the fraction of individuals that are
# correctly predicted to buy insurance is of interest
table(knn.pred, test.Y)
9/(68+9)  # 11.7% are correctly predicted "Yes"

# Fit the KNN model with K = 3
knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
5/26  # 19% are correctly predicted "Yes"

# Fit the KNN model with K = 5
knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
4/15  # 27% are correctly predicted "Yes"

# As a comparison, let's fit a logistic regression model to the data (.5 prob is the cutoff)
glm.fit <- glm(Purchase ~., data = Caravan, family = binomial, subset = -test)
glm.probs <- predict(glm.fit, Caravan[test,], type = "response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .5] = "Yes"
table(glm.pred, test.Y)   # determines the confusion matrix

# Let's fit a logistic regression model to the data (.25 prob is the cutoff)
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .25] = "Yes"
table(glm.pred, test.Y)
11/(22+11)  # 33% are correctly predicted "Yes" to purchasing insurance


## R Lab #5 Solutions

# Q1
glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
summary(glm.fit)
coef(glm.fit) # get the coefficients of the fitted model
summary(glm.fit)$coef[,4] # get the p-values for the coefficients

# Q2
106/(35+106)  # 0.752 = 75%

# Q3
35/(35+76)  # 0.315 = 32%

# Q4
max(lda.pred$posterior[,1]) # = 0.5202 = 52%
min(lda.pred$posterior[,1]) # = 0.4578 = 46%

# Q5
121 / (121 + 81)  # 0.599 = 60%

# Q6
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005) # determines the confusion matrix
#         Direction.2005
# knn.pred Down Up
#     Down   48 55
#     Up     63 86

# Q7
lda.fit=lda(Purchase~.,data=Caravan,subset=-test)
lda.probs=predict(lda.fit, Caravan[test,])$posterior[,2]
lda.pred=rep("No",1000)
lda.pred[lda.probs>.25]="Yes"
table(lda.pred,test.Y)
#          test.Y
# lda.pred  No Yes
#      No  914  46
#      Yes  27  13

# Q8
13/(27+13)  # 32.5% are correctly predicted "Yes" to purchasing insurance