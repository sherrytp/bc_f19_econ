getwd()

# Clear the workspace
rm(list = ls()) # Clear environment
gc()            # Clear unused memory
cat("\f")       # Clear the console

library(ggplot2)
library(fpp2)
library(dplyr)

# 9/10
?goog
# Closing stock prices of GOOG from the NASDAQ exchange, for 1000 consecutive trading days between 25 February 2013 and 13 February 2017. Adjusted for splits. goog200 contains the first 200 observations from goog.
?auscafe
# The total monthly expenditure on cafes, restaurants and takeaway food services in Australia ($billion). April 1982 - September 2017.
# h is the forecasting periods 
autoplot(auscafe)
mean <- meanf(auscafe, h = 20)
n <- naive(auscafe, h = 20)
sn <- snaive(auscafe)
rwf <- rwf(auscafe, h = 20)

autoplot(mean)
autoplot(n)
autoplot(sn)   # works the best 
autoplot(rwf)
# When and how you can calculate the predictive intervals? - covered in future lectures 


# 9/17
beer <- window(ausbeer, start = 1992)
fc <- snaive(beer)
autoplot(fc)

checkresiduals(fc)

# mbeer <- fc$mean
# autoplot(mbeer)
# NOTE: 1. mean is around 0 
summary(residuals(fc))
# 2. Very correlated and autocorrealted due to ACF 


# 9/24 
e <- tsCV(goog200, rwf, drift = TRUE, h=1)
sqrt(mean(e^2, na.rm = TRUE))
sqrt(mean(residuals(rwf(goog200, drift=TRUE))^2,
          na.rm=TRUE))
# Smaller RMSE 
goog200 %>% 
  tsCV(forecastingfunction = rwf, drift = TRUE, h = 1) -> e 
e^2 %>% mean(na.rm = TRUE) %>% sqrt 
goog200 %>% rwf(drift = TRUE) %>% residuals -> res 
res^2 %>% mean(na.rm = TRUE, h = 1)

library(ggplot2)
# Grouped
value <- School$Registered
ggplot(School, aes(fill=condition, y=value, x=School)) + 
  geom_bar(position="dodge", stat="identity")
# Stacked
ggplot(School, aes(fill=condition, y=value, x=School)) + 
  geom_bar(position="stack", stat="identity")

## Running Regression 
# Linear trend
fit.lin <- tslm(marathon ~ trend)
fcasts.lin <- forecast(fit.lin, h=10)
# Exponential trend
fit.exp <- tslm(marathon ~ trend, lambda = 0)
fcasts.exp <- forecast(fit.exp, h=10)
# Piecewise linear trend
t.break1 <- 1940
t.break2 <- 1980
t <- time(marathon)
t1 <- ts(pmax(0, t-t.break1), start=1897)
t2 <- ts(pmax(0, t-t.break2), start=1897)
fit.pw <- tslm(marathon ~ t + t1 + t2)
t.new <- t[length(t)] + seq(10)
t1.new <- t1[length(t1)] + seq(10)
t2.new <- t2[length(t2)] + seq(10)
newdata <- cbind(t=t.new, t1=t1.new, t2=t2.new) %>%
  as.data.frame
fcasts.pw <- forecast(fit.pw, newdata = newdata)
autoplot(fcasts.pw)
checkresiduals(fcasts.pw)
## normal distribution of residuals actually doesn't matter that much but our assumption is normally distributed. 

# Spline trend
library(splines)
t <- time(marathon)
fit.splines <- lm(marathon ~ ns(t, df=6))   # treated as degree of freedom, so the number of knots you want to pick 
summary(fit.splines)

# Spline Coefficients and f 
fc <- splinef(marathon)
autoplot(fc)

# predit US Consumption 
fit.consBest <- tslm(
  Consumption ~ Income + Savings + Unemployment,
  data = uschange)
h <- 4
newdata <- data.frame(
  Income = c(1, 1, 1, 1),
  Savings = c(0.5, 0.5, 0.5, 0.5),
  Unemployment = c(0, 0, 0, 0))
fcast.up <- forecast(fit.consBest, newdata = newdata)
newdata <- data.frame(
  Income = rep(-1, h),
  Savings = rep(-0.5, h),
  Unemployment = rep(0, h))
fcast.down <- forecast(fit.consBest, newdata = newdata)
autoplot(uschange[, 1]) +
  ylab("% change in US consumption") +
  autolayer(fcast.up, PI = TRUE, series = "increase") +
  autolayer(fcast.down, PI = TRUE, series = "decrease") +
  guides(colour = guide_legend(title = "Scenario"))


# Decomposition: 
elecequip %>%
  stl(s.window=7, t.window=11) %>%
  autoplot()
# increasing the s.window with large dataset will slow the process down; s.window = either the character string "periodic" or the span (in lags) of the loss window for seasonal extraction, which should be odd and at least 7, according to Cleveland et al. This has no default. 
# t.window = smoothing window with large range for trend; the span (in lags) of the loess window for trend extraction, which should be odd. If NULL, the default, nextodd(ceiling((1.5*period) / (1-(1.5/s.window)))), is taken.

# Oct. 22 
# X-11 Decomposition 
library(seasonal)
fit <- seas(elecequip, x11="")
autoplot(fit)

# INTERESTING GRAPH 
arrows(2,8, 7, 4, col = "red", lwd = 2)
k = -0.8 
for (i in 1:10){
  len = 1/10
  arrows(2,8,2+len, 8 - 0.8*len, col = "red")
}
