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
