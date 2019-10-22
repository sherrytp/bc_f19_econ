# Banking Reports manipulate 
rm(list=ls())

# Banking Financial Statements 
library(readxl)
Y9C_banks_Tian <- read_excel("Y9C_banks_Tian.xlsx")

citiBala <- subset(Y9C_banks_Tian, rssd9001 == 1951350, select = c("bhck0081",
        "bhck0395","bhck0397","bhck1754","bhck1773","bhdmb987", 
        "bhckb989","bhck5369","bhckb529","bhck3545","bhck2145","bhck2150","bhck2130",
        "bhck3656","bhck3163","bhck0426","bhck2160","bhck2170"))
citiBall <- subset(Y9C_banks_Tian, rssd9001 == 1951350, select = c("bhdm6631","bhdm6636","bhfn6631","bhfn6636","bhdmb993","bhckb995","bhck3548","bhck4062","bhckc699"))
citiBale <- subset(Y9C_banks_Tian, rssd9001 == 1951350, select = c("bhck3283","bhck3230","bhck3240","bhck3247","bhckb530","bhcka130"))

# no missing variables 
citiBala[1,] -> citiBala_Y1

citiBala_Y1$bhck0081+citiBala_Y1$bhck0395+citiBala_Y1$bhck0397 -> citi_cash
citiBala_Y1$bhck1754+citiBala_Y1$bhck1773 -> citi_securities
citiBala_Y1$bhdmb987 + citiBala_Y1$bhckb989 -> citi_repoA
citiBala_Y1$bhck5369+citiBala_Y1$bhckb529 -> citi_loans
citiBala_Y1$bhck3545 -> citi_tradingA
citiBala_Y1$bhck3163+citiBala_Y1$bhck0426 -> citi_otherA

c(citi_cash,citi_loans,citi_otherA,citi_repoA,citi_securities,citi_tradingA)
# [1] 156402000 633963000  29376000 239015000 319990000 302983000
sum(citi_cash,citi_loans,citi_otherA,citi_repoA,citi_securities,citi_tradingA)
# [1] 1681729000

piechart(x, labels = names(x), edges = 200, radius = 0.8,
         density = NULL, angle = 45, col = NULL, main = NULL, ...)


# Open Operational Risk file and Change colnames 
oprisk_data <- read_excel("oprisk_data.xlsx")
old.names <- colnames(oprisk_data)
old.names
new.names <- c("event", "bus", "loss", "settleD", "settleY", "settleM", "industry", "sector")
colnames(oprisk_data) <- new.names
View(oprisk_data)

# remove colnames
rm(new.names)
rm(old.names)

# group loss by industries 
agriculture <- subset(oprisk_data, industry == "11 - Agriculture, Forestry, Fishing and Hunting", select = loss) 
mining <- subset(oprisk_data, industry == "21 - Mining, Quarrying, and Oil and Gas Extraction", select = loss) 
utilities <- subset(oprisk_data, industry == "22 - Utilities", select = loss) 
construction <- subset(oprisk_data, industry == "23 - Construction", select = loss) 
manufacturing <- subset(oprisk_data, industry == "31 - Manufacturing", select = loss) 
wholefoods <- subset(oprisk_data, industry == "42 - Wholesale Trade", select = loss) 
retail <- subset(oprisk_data, industry == "44 - Retail Trade", select = loss) 
transportation <- subset(oprisk_data, industry == "48 - Transportation and Warehousing", select = loss) 
information <- subset(oprisk_data, industry == "51 - Information", select = loss) 
finance <- subset(oprisk_data, industry == "52 - Finance and Insurance", select = loss) 
realestate <- subset(oprisk_data, industry == "53 - Real Estate, Rental and Leasing", select = loss) 
professional <- subset(oprisk_data, industry == "54 - Professional, Scientific and Technical Services", select = loss) 
management <- subset(oprisk_data, industry == "55 - Management of Companies and Enterprises", select = loss) 
administrative <- subset(oprisk_data, industry == "56 - Administrative and Support, Waste Management and Remediation Services", select = loss) 
education <- subset(oprisk_data, industry == "61 - Educational Services", select = loss) 
healthcare <- subset(oprisk_data, industry == "62 - Health Care and Social Assistance", select = loss) 
arts <- subset(oprisk_data, industry == "71 - Arts, Entertainment and Recreation", select = loss) 
accommodation <- subset(oprisk_data, industry == "72 - Accommodation and Food Services", select = loss) 
others <- subset(oprisk_data, industry == "81 - Other Services (except Public Administration)", select = loss) 
public <- subset(oprisk_data, industry == "92 - Public Administration", select = loss) 

# drop business types, industry, and settlement dates 
drop.names <- c("event", "loss", "industry")
oprisk <- data.frame(oprisk_data[drop.names])
rm(drop.names)
#df <- diamonds %>%
#group_by(cut) %>%
#summarise(counts = n())
# ggplot 2, geom_bar() with option stat = "identity" is used to create the bar plot of the summary output as it is.

# group loss by events 
table(oprisk$event)
unique(oprisk$event)
# Transform the data by taking natural logarithms
oprisk$log = log(oprisk$loss)
View(oprisk)

# create groups for 7 different events 
h1 <- hist(oprisk$loss, col = "brown", xlab = "Loss", 
          main = "Histogram of Operational Risk")
xfit <- seq(min(oprisk$loss),max(oprisk$loss), length = 100) 
yfit<-dnorm(xfit,mean=mean(oprisk$loss),sd=sd(oprisk$loss)) 
yfit <- yfit*diff(h1$mids[1:2])*length(oprisk$loss) 
lines(xfit, yfit, col="blue", lwd=2)

h2 <- hist(oprisk$log, col = "brown", xlab = "Log of Loss", 
          main = "Histogram of Operational Risk in Logs")
xfit <- seq(min(oprisk$log),max(oprisk$log), length = 100) 
yfit<-dnorm(xfit,mean=mean(oprisk$log),sd=sd(oprisk$log)) 
yfit <- yfit*diff(h2$mids[1:2])*length(oprisk$log) 
lines(xfit, yfit, col="blue", lwd=2)

# group by different event types 
oprisk_data$log = oprisk$log
CPBP <- subset(oprisk_data, event == "Clients Products and Business Practices", select = c(loss,log,settleY))
EDPM <- subset(oprisk_data, event == "Execution Delivery and Process Management", select  = c(loss,log,settleY))
DPA <- subset(oprisk_data, event == "Damage to Physical Assets", select  = c(loss,log,settleY))
BDSF <- subset(oprisk_data, event == "Business Disruption and System Failures", select  = c(loss,log,settleY))
IF <- subset(oprisk_data, event == "Internal Fraud", select  = c(loss,log,settleY))
EF <- subset(oprisk_data, event == "External Fraud", select  = c(loss,log,settleY))
EPWS <- subset(oprisk_data, event == "Employment Practices and Workplace Safety", select  = c(loss,log,settleY))
# calculate loss sum 
losstable <- data.frame(loss = c(sum(CPBP$loss), sum(EDPM$loss), sum(DPA$loss), sum(BDSF$loss), sum(IF$loss), sum(EF$loss), sum(EPWS$loss)), 
                        event = c('CPBP', 'EDPM', 'DPA', 'BDSF', 'IF', 'EF', 'EPWS'))
# plot a barchart for total losses by 7 different events 
quartz()
par(mfrow = c(4,2))
barplot(losstable$loss, main = "Loss Distribution Histogram", xlab = "event type", 
        ylab = "losses", col = "darkgreen", names.arg = c('CPBP', 'EDPM', 'DPA', 'BDSF', 'IF', 'EF', 'EPWS'))
# plot density graphs for all 7 events (or histograms for distribution)
d1 <- density(CPBP$loss)
d2 <- density(EDPM$loss)
d3 <- density(DPA$loss)
d4 <- density(BDSF$loss)
d5 <- density(IF$loss)
d6 <- density(EF$loss)
d7 <- density(EPWS$loss)

plot(d1)
plot(d2)
plot(d3)
plot(d4)
plot(d5)
plot(d6)
plot(d7)
# remove all 7 density variables 
rm(d1,d2,d3,d4,d5,d6,d7)

# download the graph mattress and analysize the loss distribution 
# how to save in quartz - Error: can only print from a screen device 
#quartz.save(file = "Loss Distribution", type = "png", device = dev.cur(), dpi = 100)

# clear and quit quartz 
sessionInfo()   # should give your R version and all packages you have installed 
dev.off()
getOption("device")
options(device = "RStudioGD")
getOption("device")     # should show [1] "RStudioGD"

# create a new dataframe containing log values 
logtable <- data.frame(losslog = c(sum(CPBP$log), sum(EDPM$log), sum(DPA$log), sum(BDSF$log), sum(IF$log), sum(EF$log), sum(EPWS$log)), 
                       event = c('CPBP', 'EDPM', 'DPA', 'BDSF', 'IF', 'EF', 'EPWS'))
barplot(logtable$losslog, main = "Log of Loss Distribution", xlab = "event type", 
        ylab = "log of losses", col = "darkred", names.arg = c('CPBP', 'EDPM', 'DPA', 'BDSF', 'IF', 'EF', 'EPWS'))

# count average annual event happening 
freqtable <- data.frame(table(oprisk_data$settleY,oprisk_data$event))
freqtable$avgfreq <- sum(freqtable$event)/40
count(oprisk_data, )


# Calculate Value-at-Risk using historical approach
library(readxl)
GMstk <- read_excel("GMstk.xlsx")
View(GMstk)
#h <- hist(GMstk$`Daily Return`, col = "red", xlab = "Daily Return", 
#          main = "Histogram of Daily Return")
h <- hist(GMstk$`Daily Return`, col = "red", xlab = "Daily Return", 
          main = "Histogram of Daily Return", ylim = range(0:650))
GMstk$`Daily Return`[is.na(GMstk$`Daily Return`)] <- 0   # replace missing variable with 0, mid-point of return
rug(jitter(GMstk$`Daily Return`))

# One way from Quick-R by Datacamp
xfit <- seq(min(GMstk$`Daily Return`),max(GMstk$`Daily Return`), length = 10) 
yfit<-dnorm(xfit,mean=mean(GMstk$`Daily Return`),sd=sd(GMstk$`Daily Return`)) 
yfit <- yfit*diff(h$mids[1:2])*length(GMstk$`Daily Return`) 
lines(xfit, yfit, col="blue", lwd=2)

# Alternative way from Stackoverflow but for density line 
multiplier <- h$counts / h$density

mydensity <- density(GMstk$`Daily Return`)
mydensity$y <- mydensity$y * multiplier[1]
lines(mydensity)

# compute the confidence level 
GMstk$`Daily Return`[is.na(GMstk$`Daily Return`)] <- 0

quantile(GMstk$`Daily Return`, probs = 0.99)
quantile(GMstk$`Daily Return`, probs = 0.05)
quantile(GMstk$`Daily Return`, probs = c(0.01,0.05,0.10,0.50))

qnorm(0.99, mean = mean(GMstk$`Daily Return`),
      sd = sd(GMstk$`Daily Return`))
qnorm(0.01, mean = mean(GMstk$`Daily Return`),
      sd = sd(GMstk$`Daily Return`))

# apply normal distribution 
fit <- dnorm(GMstk$`Daily Return`, mean = mean(GMstk$`Daily Return`),
             sd = sd(GMstk$`Daily Return`))
fit <- fit*diff(h$mids[1:2])*length(GMstk$`Daily Return`) 
par(new = TRUE)    # over-writes on the previous graph 
plot(GMstk$`Daily Return`, fit)
# compute confidence level on normality 
qnorm(0.01, mean = mean(GMstk$`Daily Return`),
      sd = sd(GMstk$`Daily Return`))


# clean up variables; open, save, and close quarts()
quartz()
par(mar=c(6.1,5.1,4.1,2.1))
par("mar")
rm(h, mydensity, multiplier, xfit, yfit)
dev.off()
getOption("device")
