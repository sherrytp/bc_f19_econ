# ADEC 7430: Big Data Econometrics
# R Lab #3: Cross-Validation and the Bootstrap (pp. 190 - 197)

# Load and Plot the data
library(ISLR)

plot(mpg ~ horsepower, data = Auto,
     xlab = "Horsepower", ylab = "mpg", main = "mpg vs. horsepower",
     col = "red")

# 5.3.1 The Validation Set Approach

# Split the set of observations into two halves by selecting a random subset of 
# 196 observations out of the original 392 observations
set.seed(1)
train <- sample(392, 196)

# Fit a linear regression model from the training set
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

# Estimate the response for all 392 observations
# Calculate the MSE of the 196 obs in the validation set
# The [-train] selects only the observations not in the training set 
attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2) # MSE (Test Error) = 26.14

# Estimate the test error for the quadratic and cubic regressions
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2) # MSE (Test Error) = 19.82

lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2) # MSE (Test Error) = 19.78

# Select a different training set to obtain different errors on the validation set
set.seed(2)
train <- sample(392, 196)

lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2) # MSE (Test Error) = 23.30

lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2) # MSE (Test Error) = 18.90

lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2) # MSE (Test Error) = 19.26

# The results show that the quadratic function has a better prediction
# (i.e. lower MSE) than the linear and cubic functions

# 5.3.2 Leave-One-Out Cross-Validation (LOOCV)

glm.fit <- glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)

lm.fit <- lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)

# We use the glm() function rather than the lm() function for the regression model
library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta #24.23, 24.23 is the cross-validation LOOCV estimate for the test MSE

# We want to repeat this procedure for increasingly complex polynomial fits, so
# we use a "for loop" to automate the process

# Initialize the CV test error vector to 0
cv.error <- rep(0, 5)

for (i in 1:5){
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error #24.23151 19.24821 19.33498 19.42443 19.03321


## 5.3.3 k-Fold Cross-Validation

set.seed(17)
cv.error.10 <- rep(0, 10)
degree <- 1:10
for(d in degree){
  glm.fit <- glm(mpg ~ poly(horsepower, d), data = Auto)
  cv.error.10[d] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10
# 24.20520 19.18924 19.30662 19.33799 18.87911 19.02103 18.89609 19.71201 18.95140 19.50196

# The computation time is much shorter for k-Fold CV compared to LOOCV
# There is little evidence that using cubic or higher-order polynomial terms 
# leads to lower test error than simply using a quadratic fit.

# 5.3.4 The Bootstrap

# Estimating the accuracy of a statistic of interest
# We first create a function that computes the statistic of interest
# Then we use the boot() function to perform the bootstrap by repeatedly
# sampling from the data with replacement

# Minimum Risk Investment (Portfolio data set) - Section 5.2

# We first create the function alpha.fn() which takes as input the (X, Y)
# data as well as a vector indicating which obs should be used to estimate alpha
# The function outputs the estimate for alpha based on the selected obs
alpha.fn = function(data, index){
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y) - cov(X, Y))/(var(X) + var(Y) - 2*cov(X, Y)))
}

# Estimate alpha using all 100 observations
alpha.fn(Portfolio, 1:100) # 0.576

# Next, we randomly select 100 obs from the range 1 to 100, with replacement
# This is equivalent to constructing a new boostrap data set and recomputing alpha_hat
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T)) #0.596

# The boot function automates this approach, let R = 1,000 bootstrap estimates for alpha
boot(Portfolio, alpha.fn, R = 1000)
plot(boot(Portfolio, alpha.fn, R = 1000))
# The alpha_hat is 0.5758 and the bootstrap estimate for SE(alpha_hat) is 0.0886

# Estimating the accuracy of a linear regression model
# We create a simple function that takes in the Auto data set as well as a set of indices
# for the obs, and returns the intercept and slope estimates for the linear regression model
# We then apply this function to the full set of 392 obs in order to compute the estimates
# of Beta_0 and Beta_1 on the entire data set using the linear regression slop estimate formulas
boot.fn = function(data, index)
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
boot.fn(Auto, 1:392) #39.936, -0.158

# We can also use the boot.fn function to create bootstrap estimates for the intercept and
# slope terms by randomly sampling from among the obs with replacement
set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
boot.fn(Auto, sample(392, 392, replace = T))

# We use the boot() function to compute the standard erros of 1,000 bootstrap estimates
# for the intercept and slope terms
boot(Auto, boot.fn, 1000)
plot(boot(Auto, boot.fn, 1000))
# The bootstrap estimate for SE(Beta_0_hat) is 0.86 and for SE(Beta_1_hat) is 0.0074.

# Standard formulas can be used to compute the standard errors for the regression
# coefficients in a linear model using the summary() function
summary(lm(mpg ~ horsepower, data = Auto))$coeff # 0.717, 0.0064

# Next, we compute bootstrap SE estimates and standard linear regression estimates the result
# from fitting the quadratic model to the data.
boot.fn = function(data, index)
  coefficients(lm(mpg ~ horsepower + I(horsepower^2), data = data, subset = index))
set.seed(1)
boot(Auto, boot.fn, 1000)
plot(boot(Auto, boot.fn, 1000))
summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coeff

# R Lab #3 Solutions

# Q1
set.seed(3)
train <- sample(392, 196)
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2) # MSE (Test Error) = 26.29
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2) # MSE (Test Error) = 21.50
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2) # MSE (Test Error) = 21.51

# Q2
glm.fit <- glm(mpg ~ poly(horsepower, 6), data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta # 18.98 18.98

# Q4
set.seed(17)
cv.error.5 <- rep(0, 10)
degree <- 1:10
for(d in degree){
  glm.fit <- glm(mpg ~ poly(horsepower, d), data = Auto)
  cv.error.5[d] <- cv.glm(Auto, glm.fit, K = 5)$delta[1]
}
cv.error.5
# 24.26240 19.15424 19.14205 19.42963 18.87288 19.36350 19.04407 18.91539 19.22077 19.16847
cv.error.5[1] # 24.2624
cv.error.5[2] # 19.15424

# Q4
set.seed(2)
boot(Portfolio, alpha.fn, R = 1000)
plot(boot(Portfolio, alpha.fn, R = 1000))
# The alpha_hat is 0.5758 and the bootstrap estimate for SE(alpha_hat) is 0.090

# Q6
boot.fn = function(data, index)
  coefficients(lm(mpg ~ horsepower + I(horsepower^2), data = data, subset = index))
set.seed(2)
boot(Auto, boot.fn, 1000)
plot(boot(Auto, boot.fn, 1000))
# 2.1445, 0.0337, 0.0001