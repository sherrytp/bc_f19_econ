# ADEC 7430: Big Data Econometrics
# R Lab #2 (Ch. 7): Non-Linear Modeling (pp. 287 - 297)

# Load the relevant libraries and Wage data
library(ISLR)
library(splines)
library(gam)
library(akima)
attach(Wage)

# 7.8.1 Polynomial Regression and Step Functions

fit <- lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))
# The function returns a matrix whose columns are a basis
# of orthogonal polynomials, which essentially means that each
# column is a linear combination of the variables

fit2 <- lm(wage ~ poly(age, 4, raw = TRUE), data = Wage)
coef(summary(fit2))

fit2a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage)
coef(fit2a)

fit2b <- lm(wage ~ cbind(age, age^2, age^3, age^4), data = Wage)
coef(fit2b)

# Create a grid of values for age at which we want predictors
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

# Now we plot the data and add the fit from the degree-4 polynomial
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree-4 Polynomial", outer = TRUE)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# Whether or not an orthogonal set of basis functions is produced will not
# affect the model obtained b/c the fitted values are identical
preds2 <- predict(fit2, newdata = list(age = age.grid), se = TRUE)
max(abs(preds$fit - preds2$fit))

# In performing polynomial regression, we must decide on the degree
# of the polynomial to use. One methods is using hypothesis tests.

# We now fit models ranging from linear to a degree-5 polynomial
# and seek to determine the simplest model which is sufficient to
# explain the relationship between "wage" and "age"

# We use ANOVA (F-test) to test Ho vs. Ha
# Ho = Model M1 is sufficient to explain the data
# Ha = Model M2 (more complex model) is sufficient
fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ poly(age, 3), data = Wage)
fit.4 <- lm(wage ~ poly(age, 4), data = Wage)
fit.5 <- lm(wage ~ poly(age, 5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# The cubic or quartic polynomial appears to be a reasonable fit

# Because poly() create orthgonal polynomials, let's do:
coef(summary(fit.5))
# Note that the p-values are the same as the ANOVA, and the
# square of the t-statistics equals the F-statistics
(-11.983)^2

# We can use the ANOVA method to compare these three models:
fit.1 <- lm(wage ~ education + age, data = Wage)
fit.2 <- lm(wage ~ education + poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ education + poly(age, 3), data = Wage)
anova(fit.1, fit.2, fit.3)

# We now consider the task of predicting whether an individual
# earns more than $250,000 per year by fitting a polynomial
# logistic regression model.
fit <- glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial)
# We use the wrapper I() to create the binary response variable on the fly
preds <- predict(fit, newdata = list(age = age.grid), se = TRUE)
pfit <- exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))

# Can also directly compute the probabilities
preds <- predict(fit, newdata = list(age = age.grid), type = "response", se = TRUE)

# Plot the data
plot(age, I(wage > 250), xlim = agelims, type = "n", ylim = c(0, .2))
points(jitter(age), I((wage > 250)/5), cex = .5, pch = "|", col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# In order to fit a step function, we use the cut() function
table(cut(age, 4))
fit <- lm(wage ~ cut(age, 4), data = Wage)
coef(summary(fit))

# 7.8.2 Splines

# The bs() function generates the entire matrix of basis functions for splines with
# the specified set of knots (25, 40, 60). By default, cubic splines are produced.
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
pred <- predict(fit, newdata = list(age = age.grid), se = TRUE)
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2*pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2*pred$se, lty = "dashed")
# Recall that a cubic spline with three knots has (K + 4) 7 d.o.f.

# Now let's use the df option to produce a cubic spline with knots at uniform quantiles of the data
dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))
attr(bs(age, df = 6), "knots") # chooses knots at 25th, 50th and 75th percentiles

# Now we fit a natural spline using the ns() function, with d.o.f = 4
fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
pred2 <- predict(fit2, newdata = list(age = age.grid), se = TRUE)
lines(age.grid, pred2$fit, col = "red", lwd = 2)

# Now we fit a smoothing spline
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(age, wage, df = 16)        # uses 16 d.o.f.
fit2 <- smooth.spline(age, wage, cv = TRUE)     # finds d.o.f. using LOOCV

# if cv = FALSE, uses generalized cross-validation (an approx. to LOOCV)
fit2$df #d.o.f. = 6.794
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"), 
       col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

# Now we perform local regression using spans of .2 and .5
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression")
fit <- loess(wage ~ age, span = .2, data = Wage)
fit2 <- loess(wage ~ age, span = .5, data = Wage)
lines(age.grid, predict(fit, data.frame(age = age.grid)), col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)), col = "blue", lwd = 2)
legend("topright", legend = c("Span=0.2", "Span=0.5"), col = c("red", "blue"), lty = 1
       lwd = 2, cex = .8)

# 7.8.3 Generalized Additive Models (GAMs)

# We now fit a GAM to predict "wage" using natural spline functions of "year" and "age"
# treating "education" as a qualitative predictor
gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)

# We now fit a GAM using smoothing splines (must use GAM package)
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)

# Now let's plot these tow
par(mfrow = c(2, 3))
plot(gam.m3, se = TRUE, col = "blue")
plot.gam(gam1, se = TRUE, col = "red")
title("Generalized Additive Models", outer = TRUE)

# We can now perform a series of ANOVA tests in order to determine which of these
# three models is best: a GAM that excludes "year", a GAM that uses a linear function
# of "year", or a GAM that uses a spline function of "year"
gam.m1 <- gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data = Wage)
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
anova(gam.m1, gam.m2, gam.m3, test = "F")

# Here is a summary of the GAM fit
summary(gam.m3)
# The large p-value for "year" reinforces our conclusion from the ANOVA test that
# a linear function is adequate for this term (rather than non-linear). There is, 
# however, evidence that a non-linear term is required for "age".

# Here we make predictions on the training set 
preds <- predict(gam.m2, newdata = Wage)

# We can also use local regression fits as building blocks in a GAM
gam.lo <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education, data = Wage)
par(mfrow = c(1,1))
plot.gam(gam.lo, se = TRUE, col = "green")

# We can create interactions before calling the gam() function
gam.lo.i <- gam(wage ~ lo(year, age, span = 0.5) + education, data = Wage)

# We can plot the resulting 2-D surface
plot(gam.lo.i)

# We fit a logistic regression GAM
gam.lr <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage)
par(mfrow = c(1, 3))
plot(gam.lr, se = TRUE, col = "green")
table(education, I(wage > 250))

# Now we fit a LR GAM using all but <HS Grad category
gam.lr.s <- gam(I(wage > 250) ~  year + s(age, df = 5) + education, family = binomial, data = Wage,
                subset = (education != "1. < HS Grad"))
plot(gam.lr.s, se = TRUE, col = "green")


## R Lab #2 (Ch. 7) Solutions

# Q1
fit.1 <- lm(wage ~ education + age, data = Wage)
fit.2 <- lm(wage ~ education + poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ education + poly(age, 3), data = Wage)
anova(fit.1, fit.2, fit.3)


# Q2
coef(summary(fit.3)) #poly(age, 3)3 = 2.119808

# Q3
table(cut(age,c(0,25,40,60,80)))
fit <- lm(wage ~ cut(age,c(0,25,40,60,80)), data = Wage)
coef(summary(fit))
predict(fit, data.frame(age = seq(0:80)))

# Q4
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
predict(fit, data.frame(age = 80))
fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
predict(fit2, data.frame(age = 80))
fit3 <- smooth.spline(age, wage, cv = FALSE)
fit3$y[fit3$x==80]
fit4 <- loess(wage ~ age, span = .5, data = Wage)
predict(fit4, data.frame(age = 80))

# Q5
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data = Wage)
coef(gam.m2)

# Q6
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data = Wage)
predict(gam.m2, data.frame(year = 2006, age = 50, education = "3. Some College"))