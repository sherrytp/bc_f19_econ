# ============= Banking ========================
# ========= Midterm Group Project ==============
## ----- General Prepare 
rm(list = ls()) 
cat("\f") 
library.list <- c("zoo"
                  , "fpp2"
                  , "ggrepel"
                  , "readxl"
)
for (i in 1:length(library.list)) {
  if (!library.list[i] %in% rownames(installed.packages())) {
    install.packages(library.list[i], dependencies = TRUE)
  }
  library(library.list[i], character.only = TRUE)
}
rm(library.list, i)

## ------- Load Income Data 
income <- read_excel(file.choose())

banks <- read_excel(file.choose())

macroeco <- read.csv(file.choose()
                     , check.names = FALSE
                     , stringsAsFactors = FALSE
                     , na.strings = ""
)

old.names <- colnames(macroeco)
new.names <- tolower(old.names)
new.names <- gsub(" ", ".", new.names)
new.names <- gsub("[()]", "", new.names) # Or "\\(|\\)"
colnames(macroeco) <- new.names

colnames(macroeco)[colnames(macroeco) == "3-month.treasury.rate"] <- "three"
colnames(macroeco)[colnames(macroeco) == "5-year.treasury.yield"] <- "five"
colnames(macroeco)[colnames(macroeco) == "10-year.treasury.yield"] <- "ten"

date <- strsplit(macroeco$date, " ")
macroeco$year <- as.numeric(sapply(date, function(x) x[2]))
macroeco$quarter <- sapply(date, function(x) x[1])
macroeco$date <- paste(macroeco$year, macroeco$quarter)
macroeco$date <- as.yearqtr(macroeco$date)     # very important to ensure all "date" column are date.format 
macroeco$year <- NULL 
macroeco$quarter <- NULL 
rm(date)
colnames(macroeco)

## ------ Matching datadate of two BHC 
# add another column for recoding quarter name for two BHC

colnames(income)[colnames(income) == "X__1"] <- "date"
date <- strsplit(income$date, "Q") 
income$year <- as.numeric(sapply(date, function(x) x[1]))
income$quarter <- sapply(date, function(x) x[2])
income$date <- paste(income$year, sep = " Q", income$quarter)
income$date <- as.yearqtr(income$date)
#income$year <- NULL
#income$quarter <- NULL
income[, c(10,11)] <- NULL
rm(old.names, new.names, date)

banks$date <- as.Date(as.character(banks$rssd9999), "%Y%m%d")
banks$date <- as.yearqtr(as.character(banks$rssd9999), "%Y%m%d")

#if (citi$date == "03") {
#  citi$date <- "Q1"
#} else if (citi$date == "06") {
#  citi$date <- "Q2"
#} else if (citi$date == "09") {
#  citi$date <- "Q3"
#} else { citi$date <- "Q4"
#}


# Citigroup & JP Morgan 
citi <- banks[which(banks$rssd9001 == 1951350), ] 
jpmg <- banks[which(banks$rssd9001 == 1039502), ] 

## ------ Rename variables for Bank Variables 
colnames(citi)[colnames(citi) == "bhck4107"] <- "total.interest.income"
colnames(citi)[colnames(citi) == "bhck4073"] <- "total.interest.expense"
colnames(citi)[colnames(citi) == "bhck4074"] <- "net.interest.expense"
colnames(citi)[colnames(citi) == "bhck4079"] <- "total.noninterest.income"
colnames(citi)[colnames(citi) == "bhck4093"] <- "total.noninterest.expense"

colnames(jpmg)[colnames(jpmg) == "bhck4107"] <- "total.interest.income"
colnames(jpmg)[colnames(jpmg) == "bhck4073"] <- "total.interest.expense"
colnames(jpmg)[colnames(jpmg) == "bhck4074"] <- "net.interest.expense"
colnames(jpmg)[colnames(jpmg) == "bhck4079"] <- "total.noninterest.income"
colnames(jpmg)[colnames(jpmg) == "bhck4093"] <- "total.noninterest.expense" 

citi <- citi[c("rssd9999", "rssd9001", "rssd9007", "date", "total.interest.income"
            , "total.interest.expense", "net.interest.expense", "total.noninterest.income", "total.noninterest.expense")]
jpmg <- jpmg[c("rssd9999", "rssd9001", "rssd9007", "date", "total.interest.income"
               , "total.interest.expense", "net.interest.expense", "total.noninterest.income", "total.noninterest.expense")]

## -------- Compile and Merge datasets into "supervisionary macroeconomic factors" 
macroeco <- merge(macroeco, citi[, -c(1,2,3)], by = "date", 
                  all.x = TRUE, all.y = FALSE)
write.csv(macroeco, file = "citigroup factor.csv")

# scatterplot matrix and correlation table 
pairs(~total.interest.income + real.gdp.growth + nominal.gdp.growth + real.disposable.income.growth + 
        nominal.disposable.income.growth + unemployment.rate + cpi.inflation.rate + three + five 
      + ten + bbb.corporate.yield + mortgage.rate + prime.rate
      + dow.jones.total.stock.market.index.level + house.price.index.level
      + commercial.real.estate.price.index.level + market.volatility.index.level
      , data = macroeco, main = "Citigroup total interest income Scatterplot")
pairs(~total.interest.expense + real.gdp.growth + nominal.gdp.growth + real.disposable.income.growth + 
        nominal.disposable.income.growth + unemployment.rate + cpi.inflation.rate + three + five 
      + ten + bbb.corporate.yield + mortgage.rate + prime.rate
      + dow.jones.total.stock.market.index.level + house.price.index.level
      + commercial.real.estate.price.index.level + market.volatility.index.level
      , data = macroeco, main = "Citigroup total interest expense Scatterplot")
pairs(~total.noninterest.income + real.gdp.growth + nominal.gdp.growth + real.disposable.income.growth + 
        nominal.disposable.income.growth + unemployment.rate + cpi.inflation.rate + three + five 
      + ten + bbb.corporate.yield + mortgage.rate + prime.rate
      + dow.jones.total.stock.market.index.level + house.price.index.level
      + commercial.real.estate.price.index.level + market.volatility.index.level
      , data = macroeco, main = "Citigroup total noninterest income Scatterplot")
pairs(~total.noninterest.expense + real.gdp.growth + nominal.gdp.growth + real.disposable.income.growth + 
        nominal.disposable.income.growth + unemployment.rate + cpi.inflation.rate + three + five 
      + ten + bbb.corporate.yield + mortgage.rate + prime.rate
      + dow.jones.total.stock.market.index.level + house.price.index.level
      + commercial.real.estate.price.index.level + market.volatility.index.level
      , data = macroeco, main = "Citigroup total noninterest expense Scatterplot")

colnames(macroeco)
cor(x = macroeco[, c(2:17)], y = macroeco$total.interest.income, use = "complete.obs")
cor(x = macroeco[, c(2:17)], y = macroeco$total.interest.expense, use = "complete.obs")
cor(x = macroeco[, c(2:17)], y = macroeco$total.noninterest.income, use = "complete.obs")
cor(x = macroeco[, c(2:17)], y = macroeco$total.noninterest.expense, use = "complete.obs")

## ---------- Regression 
autoplot(macroeco$total.interest.income) +
  ggtitle("Time-Series of II") + 
  xlab("Time")
tslm(total.interest.income ~ date, data = macroeco)
macroeco$date <- as.ts(macroeco$date) 

citireg <- lm(total.interest.income ~ house.price.index.level + market.volatility.index.level 
              + nominal.gdp.growth + nominal.disposable.income.growth, data = macroeco)
summary(citireg)

citireg <- lm(total.interest.expense ~ prime.rate + three + bbb.corporate.yield + nominal.gdp.growth 
              + unemployment.rate, data = macroeco)

citireg <- lm(total.noninterest.income ~ real.gdp.growth + three + prime.rate, data = macroeco)
citireg <- lm(total.noninterest.expense ~ market.volatility.index.level + commercial.real.estate.price.index.level 
              + cpi.inflation.rate + house.price.index.level + nominal.gdp.growth
              , data = macroeco) 

## ------- Merge Macroeconomics and Income Data 
unique(income$institution.name) 
citi <- income[1:68,]
jpmg <- income[-(1:68), ]

citimacroeco <- merge(macroeco, citi[,-2], by = "date", 
                  all.x = TRUE, all.y = TRUE)
write.csv(citimacroeco, file ="Citi income.csv")

# !!!!!!!!- Alternatively, JPMorgan analysis: 
jpmgmacroeco <- merge(macroeco, jpmg[, -2], by = "date", 
                  all.x = TRUE, all.y = TRUE)
write.csv(jpmgmacroeco, file = "JPMG income.csv")

######################################################################################################
pairs(~provision.losses + real.gdp.growth + nominal.gdp.growth + real.disposable.income.growth + 
        nominal.disposable.income.growth + unemployment.rate + cpi.inflation.rate + three + five 
      + ten + bbb.corporate.yield + mortgage.rate + prime.rate
      + dow.jones.total.stock.market.index.level + house.price.index.level
      + commercial.real.estate.price.index.level + market.volatility.index.level
      , data = macroeco, main = "Citigroup Loss Scatterplot")
cor(macroeco[, -c(1,17)], use = "complete.obs")

jpmgmacroeco$date <- ts(jpmgmacroeco$date)
citimacroeco$date <- ts(citimacroeco$date)

tslm(provision.losses ~ real.gdp.growth + house.price.index.level + commercial.real.estate.price.index.level 
     + cpi.inflation.rate + unemployment.rate, data = citimacroeco)
tslm(provision.losses ~ real.gdp.growth + unemployment.rate + three + dow.jones.total.stock.market.index.level 
     + prime.rate, data = macroeco)
forecast.lm(provision.losses ~ real.gdp.growth + house.price.index.level + commercial.real.estate.price.index.level 
            + cpi.inflation.rate + unemployment.rate, data = macroeco, h = 8, level = 0.85)
tslm(provision.losses ~ date, data = macroeco)
lm(provision.losses ~ date, data = macroeco)
######################################################################################################

## _____ Module Selection Plots 

pairs(~log (total.interes.income.reported) + mortgage.rate + log(bbb.cor)
       + log(five or go w ten)
      , data = citimacroeco, main = "Citigroup total interest income Scatterplot")
tslm 

pairs(~total.interest.expense.reported + three + ten + cpi.inflation.rate 
      + house.price.index.level 
      , data = citimacroeco, main = "Citigroup total interest expense Scatterplot")

pairs(~log(total.noninterest.income.reported) + log(house.price index.level real)
      real.gdp.growth + marekt.volititlity 
      , data = citimacroeco, main = "Citigroup total noninterest income Scatterplot")

pairs(~total.noninterest.expense.reported + real.gdp.growth + cpi.inflation.rate
      + commercial.real.estate.price.index.level + house.price.index.level
      , data = citimacroeco, main = "Citigroup total noninterest expense Scatterplot")

pairs(~total.interes.income.reported + mortgage.rate + prime.rate + house.price.index.level
      + commercial.real.estate.price.index.level
      , data = jpmgmacroeco, main = "JPMorgan total interest income Scatterplot")

pairs(~total.interest.expense.reported + house.price.index.level + commercial.real.estate.price.index.level 
      + cpi.inflation.rate, data = jpmgmacroeco, main = "JPMorgan total interest expense Scatterplot")

pairs(~total.noninterest.income.reported + mortgage.rate + bbb.corporate.yield + dow.jones.total.stock.market.index.level 
      + house.price.index.level + market.volatility.index.level + bbb.corporate.yield
      , data = jpmgmacroeco, main = "JPMorgan total noninterest income Scatterplot")

pairs(~total.noninterest.expense.reported + house.price.index.level
      + commercial.real.estate.price.index.level + cpi.inflation.rate
      , data = jpmgmacroeco, main = "JPMorgan total noninterest expense Scatterplot")

log(loss) - gdp + log(ten) + log(mortgage.rate) + log(bbb.corporate.rate)
# then change it to log(loss) ~ gdp + log(house.price.index.level) + market.volitiltiy 
pairs(~provision.losses + commercial.real.estate.price.index.level + market.volatility.index.level 
      + dow.jones.total.stock.market.index.level + unemployment.rate + real.gdp.growth 
      , data = jpmgmacroeco, main = "JPMorgan Losses Scatterplot")
pairs(~provision.losses + house.price.index.level + market.volatility.index.level 
      + cpi.inflation.rate + unemployment.rate + real.gdp.growth 
      , data = citimacroeco, main = "Citigroup Losses Scatterplot")

### Data Import and Cleaning 
## Including Plots
## Import Hist_Income Data 
library(readxl)
library(zoo)
library(fpp2)
income <- read_excel("Income_Hist_Data.xlsx")
macro <- read.csv("Supervisory Severely Adverse Domestic.csv")

unique(income$institution.name) 
colnames(income)[colnames(income) == "X__1"] <- "date"
date <- strsplit(income$date, "Q") 
income$year <- as.numeric(sapply(date, function(x) x[1]))
income$quarter <- sapply(date, function(x) x[2])
income$date <- paste(income$year, sep = " Q", income$quarter)
income$date <- as.yearqtr(income$date)

income[, c(10,11)] <- NULL


### Data Exploration 
#### Summary Table 

citi <- income[1:68,]
macroeco <- merge(macroeco, citi[, c("date", "provision.losses")], by = "date", 
                  all.x = TRUE, all.y = TRUE)
pairs(~provision.losses + real.gdp.growth + nominal.gdp.growth + real.disposable.income.growth + 
        nominal.disposable.income.growth + unemployment.rate + cpi.inflation.rate + three + five 
      + ten + bbb.corporate.yield + mortgage.rate + prime.rate
      + dow.jones.total.stock.market.index.level + house.price.index.level
      + commercial.real.estate.price.index.level + market.volatility.index.level
      , data = macroeco, main = "Citigroup Loss Scatterplot")
cor(macroeco[, -c(1,17)], use = "complete.obs")

# Run Time-Series Regression 
macroeco$date <- ts(macroeco$date)
citireg <- tslm(provision.losses ~ real.gdp.growth + unemployment.rate + three
                + dow.jones.total.stock.market.index.level + prime.rate, data = macroeco)
summary(citireg) 


jpmg <- income[-(1:68), ]
macroeco <- merge(macroeco, jpmg[, c("date", "provision.losses")], by = "date", 
                  all.x = TRUE, all.y = TRUE)
pairs(~provision.losses + real.gdp.growth + nominal.gdp.growth + real.disposable.income.growth + 
        nominal.disposable.income.growth + unemployment.rate + cpi.inflation.rate + three + five 
      + ten + bbb.corporate.yield + mortgage.rate + prime.rate
      + dow.jones.total.stock.market.index.level + house.price.index.level
      + commercial.real.estate.price.index.level + market.volatility.index.level
      , data = macroeco, main = "JPMorgan Loss Scatterplot")
cor(x = macroeco[,-1], y = macroeco[,"provision.losses"], use = "complete.obs")

macroeco$date <- ts(macroeco$date)
jpmgreg <- tslm(provision.losses ~ real.gdp.growth + market.volatility.index.level 
                + unemployment.rate + three + prime.rate, data = macroeco)
summary(jpmgreg)

# can use nrow() to create a loop? 