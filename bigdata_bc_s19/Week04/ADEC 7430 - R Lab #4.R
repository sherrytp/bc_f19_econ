# ADEC 7430: Big Data Econometrics
# R Lab #4: Subset Selection Methods, Ridge Regression, Lasso, PCR, PLS (pp. 244 - 258)

## 6.5 Lab: Subset Selection Methods

# 6.5.1 Best Subset Selection Methods
library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary)) # Salary is missing 59 players
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

# Perform best subset selection by identifying the best model that 
# contains a given number of predictors (best is quantified using RSS)
library(leaps)
regfit.full <- regsubsets(Salary ~ ., data = Hitters)
summary(regfit.full)
# An asterisk indicates that a given variable is included in the corresponding model
# The numbers on the left correspond to the # of predictors in the model

regfit.full <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
reg.summary <- summary(regfit.full)
names(reg.summary)

# Find the RSq statistic, which increases monotonically as more variables are added
reg.summary$rsq

# Now we plot the RSS and Adjusted RSq for all of the models
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Predictors", ylab = "RSS", type = "l")
which.min(reg.summary$rss)
points(19, reg.summary$rss[12], col = "blue", cex = 2, pch = 20)

plot(reg.summary$adjr2, xlab = "Number of Predictors", ylab = "Adjusted RSq", type = "l")
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)

# Now we plot the Cp and BIC for all of the models
plot(reg.summary$cp, xlab = "Number of Predictors", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "purple", cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of Predictors", ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], col = "brown", cex = 2, pch = 20)

# Or we can use the built-in plot() command to display the selected variables
# for the best model with a given # of predictors, ranked according to BIC, Cp,
# Adjusted RSq, or AIC
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

# Let's get the estimated regression coefficients for the model
coef(regfit.full, 6)

# 6.5.2 Forward and Backward Stepwise Selection

regfit.fwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

# 6.5.3 Choose Among Models Using the Validation Set Approach and Cross-Validation

# Note that we must use ONLY THE TRAINING OBSERVATIONS

# Conduct the Validation Set Approach
# Split the observations into a training set and a test set
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test <- (!train)

# We now apply regsubsets() to the training set to perform best subset selection
regfit.best <- regsubsets(Salary ~ ., data = Hitters[train,], nvmax = 19)

# We now compute the validation set error for the best model of each model size
test.mat <- model.matrix(Salary ~ ., data = Hitters[test,])

# We now loop over each size i to get the coeff. from regfit.best for the best model 
# of that size, multiply them into the appropriate columns of the test model matrix
# to form predictions, and compute the test MSE.
val.errors <- rep(NA, 19)
for(i in 1:19){
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)]%*%coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}

# We find that the best model is the one that contains ten variables
val.errors
which.min(val.errors)
coef(regfit.best, 10)

# Let's write our own predict method
predict.regsubsets <- function(object, newdata, id,...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars]%*%coefi
}

# It is important that we use the full data set in order to obtain more accurate
# coefficient estimates
regfit.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(regfit.best, 10)

# We now try to choose among the models of different sizes using cross-validation
# We perform best subset selection within each of the k training sets
k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace = T)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for (j in 1:k) {
  best.fit = regsubsets(Salary ~ ., data = Hitters[folds != j, ], nvmax = 19)
  for (i in 1:19) {
    pred = predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] = mean((Hitters$Salary[folds == j] - pred)^2)
  }
}

# This gives us a 10x19 matrix, of which the (i, j)th element corresponds
# to the test MSE for the ith cross-validation fold for the best j-variable model
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow = c(1,1))
plot(mean.cv.errors, type = 'b')
rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch = 19, type = "b")

# The cross-validation selects an 11-variable model, so we perform best subset
# selection on the full data set to get the best 11-variable model
reg.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(reg.best, 11)


## 6.6 Lab: Ridge Regression and the Lasso

# Create a matrix corresponding to the 19 predictors, which
# automatically transforms any qualitative variables into dummy variables
x <- model.matrix(Salary ~ . , data = Hitters)[,-1]
y <- Hitters$Salary

# 6.6.1 Ridge Regression
library(glmnet)

# Implement the function over a grid of values ranging from
# lambda = 10^10 to 10^-2
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)

# Get the matrix of ridge coefficients (20 x 100)
dim(coef(ridge.mod))

# When lambda = 11498, here are the coefficients and L2 norm
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))

# When lambda = 705, here are the coefficients and L2 norm
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))

# Use the predict() function to obtain ridge regression coefficients
# for a new value of lambda, say 50
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]

# We now split the samples into a training set and test set
# in order to estimate the test error of ridge regression and the lasso
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

# Now we fit a ridge regression on the training set, and evaluate its MSE
# on the test set, using lambda = 4
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test,])
mean((ridge.pred - y.test)^2) #101036.8 = test MSE
sd((ridge.pred - y.test)^2)/sqrt(length(y.test)) #SE = 16509.41

# Fit a model with just an intercept (coefficients = 0)
mean((mean(y[train]) - y.test)^2) # test MSE = 193253.1

# We could also get the same result with a VERY large value of lambda
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test,])
mean((ridge.pred - y.test)^2) # test MSE = 193253.1
sd((ridge.pred - y.test)^2)/sqrt(length(y.test)) #SE = 35874.1

# Compare ridge regression with ordinary least squares
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test,], exact = T)
mean((ridge.pred - y.test)^2) # test MSE = 114783.1
lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = T, type = "coefficients")[1:20,]

# Now we use cross-validatoin to choose the tuning parameter lambda
# The default is 10-fold CV, but can change using 'folds' argument
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam # lambda = 211.74 = 212 (leads to smallest CV error)

# Let's compute the test MSE associated with this lambda
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred - y.test)^2) # test MSE = 96015.51

# Now we refit our ridge regression model on the full data set, using
# the value of lambda chosen by cross-validation and examine coeff. estimates
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20,]

# 6.6.2 The Lasso

lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod, xvar = "lambda", label = TRUE)
# Depending on the value of lambda, some coeff. equal 0

# Now perform cross-validation and compute the test error
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y.test)^2) # test MSE = 100743.4

# Here we see that 12 of the 19 coeff. estimates equal 0
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef != 0]


## 6.7 Lab: Principal Components Regression and Partial Least Squares Regression

# 6.7.1 Principal Components Regression

## Apply PCR to the Hitters data, in order to predict Salary
# ensure that the predictors are standardized
# compute the 10-fold CV error for each possible value of M (# of principal components)
library(pls)
set.seed(2)
pcr.fit <- pcr(Salary ~ ., data = Hitters, scale = TRUE, validation = "CV")
summary(pcr.fit) # report the RMSE (so square this value to get MSE)

# Plot the cross-validation scores (MSE)
validationplot(pcr.fit, val.type = "MSEP")

# We now perform PCR on the training data and evaluate its test set performance
set.seed(1)
pcr.fit <- pcr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

# We find the lowest CV error when M = 7 component are used. Now compute the test MSE.
pcr.pred <- predict(pcr.fit, x[test,], ncomp = 7)
mean((pcr.pred - y.test)^2) # 96556.22
sd((pcr.pred - y.test)^2)/ sqrt(length(y.test)) # 15813.88

# Now we fit PCR on the full data set, using M = 7
pcr.fit <- pcr(y ~ x, scale = T, ncomp = 7)
summary(pcr.fit)

# 6.7.2 Partial Least Squares

## Now we implement PLS
set.seed(1)
pls.fit <- plsr(Salary ~ ., data = Hitters, subset = train, scale = T, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

# The lowest CV error occurs when M = 2 PLS directions.
# We now evaluate the test set MSE
pls.pred <- predict(pls.fit, x[test,], ncomp = 2)
mean((pls.pred - y.test)^2) # 101417.5

# We now perform PLS using the full data set, with M = 2
pls.fit <- plsr(Salary ~ ., data = Hitters, scale = T, ncomp = 2)
summary(pls.fit)


## R Lab #4 Solutions

# Q1
coef(regfit.full, 9)

# Q2
reg.summary$adjr2[9]
reg.summary$cp[9]
reg.summary$bic[9]

# Q3
coef(regfit.fwd, 8)
coef(regfit.bwd, 8)

# Q4
regfit.seq <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "seqrep")
summary(regfit.seq)
coef(regfit.seq, 7)

# Q5
val.errors <- rep(NA, 19)
for(i in 1:19){
  pred <- predict(regfit.best, Hitters[test, ], i)
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}
val.errors
coef(regfit.best, 3)

# Q6
k <- 5
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace = T)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for (j in 1:k) {
  best.fit = regsubsets(Salary ~ ., data = Hitters[folds != j, ], nvmax = 19)
  for (i in 1:19) {
    pred = predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] = mean((Hitters$Salary[folds == j] - pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
which.min(mean.cv.errors)

# Q7
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 50, newx = x[test,])
mean((ridge.pred - y.test)^2) # Test MSE = 97015.36

# Q8
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
plot(ridge.mod, xvar = "lambda", label = TRUE)

# Q9
?cv.glmnet
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
largelam <- cv.out$lambda.1se
largelam # lambda = 7971.935 = 7972
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = largelam, newx = x[test,])
mean((ridge.pred - y.test)^2) # test MSE = 149047.8 = 149048

# Q10
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam # 16.78016
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y.test)^2) # 100743.4
out <- glmnet(x, y, alpha = 1,lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef != 0]

# Q11
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
largelam <- cv.out$lambda.1se
largelam # 129.9227 
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
lasso.pred <- predict(lasso.mod, s = largelam, newx = x[test,])
mean((lasso.pred - y.test)^2) # test MSE = 142495.4
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = largelam)[1:20,]
lasso.coef
lasso.coef[lasso.coef != 0]

# Q12
set.seed(2)
pcr.fit <- pcr(Salary ~ ., data = Hitters, scale = TRUE, validation = "CV")
summary(pcr.fit) 

# Q13
set.seed(2)
pcr.fit <- pcr(Salary ~ ., data = Hitters, scale = TRUE, validation = "CV")
MSEP(pcr.fit)

# Q14
set.seed(1)
pcr.fit <- pcr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
pcr.pred <- predict(pcr.fit, x[test,], ncomp = 8)
mean((pcr.pred - y.test)^2) # 102538.1

# Q15
pcr.fit <- pcr(y ~ x, scale = T, ncomp = 7)
summary(pcr.fit)
plot(pcr.fit, ncomp = 1:7)
# the plots are all quite similar with very little evidence of any linear association

# Q16
set.seed(1)
pls.fit <- plsr(Salary ~ ., data = Hitters, subset = train, scale = T, validation = "CV")
summary(pls.fit)

# Q17
set.seed(1)
pls.fit <- plsr(Salary ~ ., data = Hitters, subset = train, scale = T, validation = "CV")
MSEP(pls.fit)

# Q18
pls.pred <- predict(pls.fit, x[test,], ncomp = 3)
mean((pls.pred - y.test)^2) # 100861.5

# Q19
pls.fit <- plsr(Salary ~ ., data = Hitters, scale = T, ncomp = 3)
summary(pls.fit)
# TRAINING: % variance explained
#         1 comps  2 comps  3 comps
# X         38.08    51.03    65.98
# Salary    43.05    46.40    47.72