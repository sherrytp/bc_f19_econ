getwd()

# Clear the workspace
rm(list = ls()) # Clear environment
gc()            # Clear unused memory
cat("\f")       # Clear the console

library(ggplot2)
library(fpp2)

# 2.10.1 
?gold
# Daily morning gold prices in US dollars. 1 January 1985 – 31 March 1989.
?woolyrnq
# Quarterly production of woollen yarn in Australia: tonnes. Mar 1965 – Sep 1994.
?gas
# Australian monthly gas production: 1956–1995.
autoplot(gold)
autoplot(woolyrnq)
autoplot(gas)

frequency(gold)
frequency(woolyrnq)
frequency(gas)
which.max(gold)
which.max(woolyrnq)
which.max(gas)

# 2.10.2
# a. 
tute1 <- read.csv("/Users/apple/Downloads/tute1.csv", header = TRUE)
# b. 
mytimeseries <- ts(tute1[,-1], start = 1981, frequency = 4)
# The [,-1] removes the first column which contains the quarters. 
# c. 
autoplot(mytimeseries, facets = TRUE)

# 2.10.3
# a. 
retaildata <- readxl::read_excel("/Users/apple/Downloads/retail.xlsx", skip = 1)
# skip=1 is required because the Excel sheet has two header rows 
samplets <- ts(retaildata[, "A3349873A"], 
               frequency = 12, start = c(1982, 4))
# b. 
myts <- ts(retaildata[, "A3349588R"], 
           frequency = 12, start = c(1982, 4))
# c. 
autoplot(myts)
ggseasonplot(myts)
ggsubseriesplot(myts)   # might be the best
gglagplot(myts)
ggAcf(myts)

# 2.10.4
# ?????? time plots of following time series? 
help(bicoal)
autoplot(bicoal)

help(chicken)
autoplot(chicken)

help(dole)
autoplot(dole)

help(usdealths)
autoplot(usdealths)

help(lynx)
autoplot(lynx)
# Seasonality of around 40 years in a big run and every 5 years of the smaller seasonality. 

help("goog")
autoplot(goog) + ylab("Stock Price in Dollars $") + ggtitle("Google Stock")

help(writing)
autoplot(writing)

help(fancy)
autoplot(fancy)

help(a10)
autoplot(h02)

# 2.10.5
ggseasonplot(writing)
ggseasonplot(fancy)
ggseasonplot(a10)
ggseasonplot(h02)

ggsubseriesplot(writing)
ggsubseriesplot(fancy)
ggsubseriesplot(a10)
ggsubseriesplot(h02)
# ????? Unusual years and seasonal patterns 

# 2.10.6 
autoplot(hsales)
ggseasonplot(hsales)
ggsubseriesplot(hsales)
gglagplot(hsales)
ggAcf(hsales)
# US Sales, cyclical where the trend shown not fit to a certain period every year. 
# Some repetition of seasonality around. 

autoplot(usdeaths)
ggseasonplot(usdeaths)
ggsubseriesplot(usdeaths)
gglagplot(usdeaths)
ggAcf(usdeaths)

autoplot(bricksq)
ggseasonplot(bricksq)
ggsubseriesplot(bricksq)
gglagplot(bricksq)
ggAcf(bricksq)
# Trend of growth overall; seasonality before 1975s every year; cyclical pattern similar to business cycles not in fixed time periods. 

autoplot(sunspotarea)
ggseasonplot(sunspotarea)
ggsubseriesplot(sunspotarea)
gglagplot(sunspotarea)
ggAcf(sunspotarea)

autoplot(gasoline)
ggseasonplot(gasoline)
ggsubseriesplot(gasoline)
gglagplot(gasoline)
ggAcf(gasoline)

# 2.10.7
autoplot(arrivals)
ggseasonplot(arrivals)
ggsubseriesplot(arrivals)
