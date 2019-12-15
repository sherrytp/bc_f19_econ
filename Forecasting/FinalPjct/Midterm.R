getwd()

# Clear the workspace
rm(list = ls()) # Clear environment
gc()            # Clear unused memory
cat("\f")       # Clear the console

library(ggplot2)
library(fpp2)
library(dplyr)
library(tidyr)
library(readxl)
library(stargazer)

# Import Data 
df <- read_excel("untitled folder/Europe_Policy_Uncertainty_Data.xlsx")
df <- na_if(df, "-")
colnames(df) <- df %>% 
  colnames(.) %>% 
  gsub(pattern = "_", replacement = "-")

target2bal <- df[169:390,1:22]
is.na(target2bal$Greece)
is.na(target2bal$`Greece-policy-uncertainty`)

# Descriptive
sapply(target2bal, function(x) sum(is.na(x)))
table1 <- target2bal[3:22] %>% 
  # Find the mean, st. dev., min, and max for each variable 
  summarise_each(funs(mean, sd, min, max)) %>% 
  
  # Move summary stats to columns
  gather(key, value, everything()) %>% 
  separate(key, into = c("variable", "stat"), sep = "_") %>%
  spread(stat, value) %>%
  
  # Set order of summary statistics 
  select(variable, mean, sd, min, max) %>% 
  t()

#write.table(table1, file = "summary table.txt", sep = ",", quote = FALSE, row.names = T)

# Distribution 
target2bal[3:9] %>% 
  select_if(is.numeric) %>% 
  boxplot()

target2bal[10:18] %>% 
  select_if(is.numeric) %>% 
  boxplot()
target2bal[20:21] %>% 
  select_if(is.numeric) %>% 
  boxplot()

# validation 
myts <- ts(target2bal$Greece, start = c(2001,1), frequency = 12)
fr <- ts(target2bal$France, start = c(2001, 1), frequency = 12)
gm <- ts(target2bal$Germany, start = c(2001, 1), frequency = 12)
irl <- ts(target2bal$Ireland, start = c(2001, 1), frequency = 12)
itl <- ts(target2bal$Italy, start = c(2001, 1), frequency = 12)
nl <- ts(target2bal$Netherlands, start = c(2001, 1), frequency = 12)
sp <- ts(target2bal$Spain, start = c(2001, 1), frequency = 12)
sw <- ts(target2bal$Sweden, start = c(2001, 1), frequency = 12)
uk <- ts(target2bal$UK, start = c(2001, 1), frequency = 12)
au <- ts(target2bal$Austria, start = c(2001, 1), frequency = 12)
bg <- ts(target2bal$Belgium, start = c(2001, 1), frequency = 12)

lambda <- BoxCox.lambda(bg)
autoplot(BoxCox(bg, lambda))
autoplot(bg)
lambda  # -0.4160249

myts %>% decompose(type = "additive") %>% 
  autoplot() + xlab("Year") 

myts %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition")

bg.train <- window(bg, end = c(2014,12))
bg.test <- window(bg, start = 2015) 
bg.train %>% 
  auto.arima() %>% forecast(h = 15) -> rm
accuracy(rm, bg.test)
autoplot(rm)
checkresiduals(rm)

# Seasonal naive method 
# Split the train and test sets 
myts.train <- window(myts, end = c(2014,12))
myts.test <- window(myts, start = 2015)
autoplot(myts) + 
  autolayer(myts.train, series = "Train") + 
  autolayer(myts.test, series = "Test")

# Seasonal Decomposition/Transformation
ht <- holt(myts, h=15)
ht2 <- holt(myts, damped=TRUE, phi = 0.9, h=15)
autoplot(myts) +
  autolayer(ht, series="Holt's method", PI=FALSE) +
  autolayer(ht2, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  guides(colour=guide_legend(title="Forecast"))
autoplot(ht2)

myts.train %>% 
  hw(., seasonal = "multiplicative") -> hw
accuracy(hw, myts.test)

# no damped is better 
myts.train %>% 
  hw(., seasonal = "multiplicative", damped = TRUE) -> hw2
accuracy(hw2, myts.test)

myts.train %>% 
  ets() %>% forecast(h = 15) -> et
accuracy(et, myts.test)

myts.train %>% 
  auto.arima() %>% forecast(h = 15) -> rm
accuracy(rm, myts.test) %>% stargazer()
autoplot(rm)
checkresiduals(rm)

lambda <- BoxCox.lambda(myts.train)
BoxCox(myts.train, lambda) %>% snaive(., h = 4*12) -> sn
autoplot(sn)
checkresiduals(sn)
confusionMatrix(sn, myts.test)
accuracy(sn, myts.test) %>% stargazer()

# stargazer to print out summary statistics 
stargazer(summary(rm)
  # Type of output - text, HTML or LaTeX
          , summary = FALSE   # We want summary only
          , flip = FALSE
          , title = "Auto ARIMA Forecasts" # Title of my output
          , digits = 2
)
# Regression 
lm(myts ~ target2bal$`European-News-Index` + target2bal$`Greece-policy-uncertainty`) -> table2
tslm(myts ~ target2bal$`European-News-Index` + target2bal$`Greece-policy-uncertainty`)
