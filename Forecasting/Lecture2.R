getwd()

# Clear the workspace
rm(list = ls()) # Clear environment
gc()            # Clear unused memory
cat("\f")       # Clear the console

library(ggplot2)
library(fpp2)

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