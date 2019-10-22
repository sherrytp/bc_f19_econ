# ADEC 7430: Big Data Econometrics
# R Lab #2 (Ch. 3): Linear Regression (pp. 109 - 119)

# 3.6.1 Libraries

library(MASS)
library(ISLR)

# 3.6.2 Simple Linear Regression

fix(Boston)
names(Boston)

lm.fit <- lm(medv ~ lstat, data = Boston)
attach(Boston)
lm.fit <- lm(medv ~ lstat) # Linear regression model
lm.fit
summary(lm.fit) # Summary of the linear regression model
names(lm.fit) # Find out what other pieces of information are stored
coef(lm.fit) # Extract the estimated regression coefficients
confint(lm.fit) # Obtain a confidence interval for the coefficient estimates

# We used the predict() function to produce confidence and prediction intervals
# for the prediction of medv for a given value of lstat
predict(lm.fit, data.frame(lstat <- (c(5, 10, 15))), interval = "confidence")
predict(lm.fit, data.frame(lstat <- (c(5, 10, 15))), interval = "prediction")

# Now we plot medv and lstat along with the least squares regression line
# Note that there is evidence for non-linearity in the relationship
lstat <- Boston$lstat
plot(lstat, medv)
abline(lm.fit)
abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = "red")
plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)

# Next we examine some diagnostic plots
par(mfrow = c(2, 2))
plot(lm.fit)

# From the "Residuals vs. Fitted Values" plot, we can clearly see the non-linear relationship
# Also, the normal probability plot (QQ) confirms non-linearity

# We can also compute the residuals and studentized residuals
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

# We can also compute leverage statistics for any number of predictors
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# 3.6.3 Multiple Linear Regression

lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

# Let's fit a model on all of the predictors
lm.fit <- lm(medv ~., data = Boston)
summary(lm.fit)

# Get the R^2 and RSE values
summary(lm.fit)$r.sq
summary(lm.fit)$sigma

# Compute the variance inflation factors (VIF) to determine collinearity
library(car)
vif(lm.fit)
# Note that then of a VIF > 10, so no collinearity is evident between predictors

# Perform a regression excluding only one specific predictor
lm.fit1 <- lm(medv ~. -age, data = Boston)
summary(lm.fit1)

# Or we can use the update() function
lm.fit1 <- update(lm.fit, ~.-age)

# 3.6.4 Regression with Interaction Terms

# The syntax lstat:black includes solely the interaction term
# The syntax lstat*age includes the main effects and the interactions
summary(lm(medv ~ lstat*age, data = Boston))

# 3.6.5 Non-linear Transformations of the Predictors

# We can create a predictor X^2 using I(x^2)
lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)

# We use the anova() function to further quantify the extent to which
# the quadratic fit is superior to the linear fit
lm.fit <- lm(medv ~ lstat)
anova(lm.fit, lm.fit2)
# The anova() function performs a hypothesis test comparing the two models
# The null hypothesis is that the two models fit the data equally well
# The alternative hypothesis is that the full model is superior
# Due to a small p-value, the Ho is rejected.

# Plot the quadratic model
par(mfrow = c(2, 2))
plot(lm.fit2)

# Now let's produce a fifth-order polynomial fit
lm.fit5 <- lm(medv ~ poly(lstat, 5))
summary(lm.fit5)

# Now let's try a log transformation of the predictors
summary(lm(medv ~ log(rm), data = Boston))
coef(lm(medv ~ log(rm), data = Boston))

# 3.6.6 Qualitative Predictors

fix(Carseats)
names(Carseats)
# Note that R generates dummy variables automatically

lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

# The contrasts() function returns the coding the R uses for the dummary variables
attach(Carseats)
contrasts(ShelveLoc)

# 3.6.7 Writing Functions
LoadLibraries = function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}

LoadLibraries()

# R Lab #2 (Ch. 3) Solutions

# Q1
lm.fit <- lm(medv ~ lstat, data = Boston)
predict(lm.fit, data.frame(lstat = 20), interval = "prediction")
#        fit      lwr      upr
# 1 15.55285 3.316021 27.78969

# Q2
lm.fit1 <- lm(medv ~. -age, data = Boston)
summary(lm.fit1)
which.max(summary(lm.fit1)$coefficients[, 4])
#indus 
#    4 

# Q3
rstudent(lm.fit1)[which.max(abs(rstudent(lm.fit1)))]
#    369 
#5.88543 
hatvalues(lm.fit1)[which.max(hatvalues(lm.fit1))]
#      381 
#0.3055797 
cooks.distance(lm.fit1)[which.max(cooks.distance(lm.fit1))]
#      369 
#0.1485981 

# Q4
lm.model1 <- lm(medv ~ lstat*age, data = Boston)
lm.model2 <- lm(medv ~ lstat*black, data = Boston)
c(summary(lm.model1)$sigma, summary(lm.model1)$r.squared)
# [1] 6.1485133 0.5557265
c(summary(lm.model2)$sigma, summary(lm.model2)$r.squared)
# [1] 6.109289 0.561377

# Q5
summary(lm(medv ~ log(rm), data = Boston))$coefficients[2, 1]
# [1] 54.05457

# Q6
lm.fit.new <- lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats,
                 contrasts = list(ShelveLoc=contr.treatment(c("Bad", "Good", "Medium"), base = 3)))
summary(lm.fit.new)$coefficients[7:8, 1]
#ShelveLocBad ShelveLocGood 
#   -1.953262      2.895414 