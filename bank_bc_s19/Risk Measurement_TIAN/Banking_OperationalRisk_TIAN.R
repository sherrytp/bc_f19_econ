# Banking Reports manipulate 
rm(list=ls())

# Open Operational Risk file and Change colnames 
oprisk_data <- read_excel("~/Desktop/oprisk_data.xlsx")
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
oprisk_data$log = oprisk$log

# histogram of normal distribution 
h1 <- hist(oprisk$loss, col = "brown", xlab = "Loss", 
           main = "Histogram of Operational Risk")
xfit <- seq(min(oprisk$loss),max(oprisk$loss), length = 100) 
yfit<-dnorm(xfit,mean=mean(oprisk$loss),sd=sd(oprisk$loss)) 
yfit <- yfit*diff(h1$mids[1:2])*length(oprisk$loss) 
lines(xfit, yfit, col="blue", lwd=2)

# histogram of normal distribution in logs 
h2 <- hist(oprisk$log, col = "brown", xlab = "Log of Loss", 
           main = "Histogram of Operational Risk in Logs")
xfit <- seq(min(oprisk$log),max(oprisk$log), length = 100) 
yfit<-dnorm(xfit,mean=mean(oprisk$log),sd=sd(oprisk$log)) 
yfit <- yfit*diff(h2$mids[1:2])*length(oprisk$log) 
lines(xfit, yfit, col="blue", lwd=2)

# get datasets for each event type 
CPBP <- oprisk[oprisk$event == "Clients Products and Business Practices",]
EDPM <- oprisk[oprisk$event == "Execution Delivery and Process Management",]
DPA <- oprisk[oprisk$event == "Damage to Physical Assets",]
BDSF <- oprisk[oprisk$event == "Business Disruption and System Failures",]
IF <- oprisk[oprisk$event == "Internal Fraud",]
EF <- oprisk[oprisk$event == "External Fraud",]
EPWS <- oprisk[oprisk$event == "Employment Practices and Workplace Safety",]

# get the parameters of the normal distribution
mean(BDSF$log)
sd(BDSF$log)

mean(CPBP$log)
sd(CPBP$log)

mean(DPA$log)
sd(DPA$log)

mean(EDPM$log)
sd(EDPM$log)

mean(EF$log)
sd(CPBP$log)

mean(EPWS$log)
sd(EPWS$log)

mean(IF$log)
sd(IF$log)

# get 99.9th percentile 
quantile(BDSF$log, probs = 0.999)
quantile(CPBP$log, probs = 0.999)
quantile(DPA$log, probs = 0.999)
quantile(EDPM$log, probs = 0.999)
quantile(EF$log, probs = 0.999)
quantile(EPWS$log, probs = 0.999)
quantile(IF$log, probs = 0.999)

rm(xfit, yfit, h1, h2)
# generate a new dataset of industry code-52
finance <- subset(oprisk_data, industry == "52 - Finance and Insurance", select = event & loss & log)
barplot(finance$loss, main = "Loss Distribution of FIN", xlab = "event type", 
        ylab = "losses of FIN", col = "darkgreen", names.arg = c('CPBP', 'EDPM', 'DPA', 'BDSF', 'IF', 'EF', 'EPWS'))

# histogram of normal distribution 
h1 <- hist(finance$loss, col = "red", xlab = "Loss", 
           main = "Histogram of Operational Risk - Finance")
xfit <- seq(min(finance$loss),max(finance$loss), length = 100) 
yfit<-dnorm(xfit,mean=mean(finance$loss),sd=sd(finance$loss)) 
yfit <- yfit*diff(h1$mids[1:2])*length(finance$loss) 
lines(xfit, yfit, col="blue", lwd=2)

# histogram of normal distribution in logs 
h2 <- hist(finance$log, col = "red", xlab = "Log of Loss", 
           main = "Histogram of Operational Risk in Logs - Finance")
xfit <- seq(min(finance$log),max(finance$log), length = 100) 
yfit<-dnorm(xfit,mean=mean(finance$log),sd=sd(finance$log)) 
yfit <- yfit*diff(h2$mids[1:2])*length(finance$log) 
lines(xfit, yfit, col="blue", lwd=2)

# split datasets by event type 
table(finance$event)    # make sure we have the correct number for each event type 
CPBP <- finance[finance$event == "Clients Products and Business Practices",]
EDPM <- finance[finance$event == "Execution Delivery and Process Management",]
DPA <- finance[finance$event == "Damage to Physical Assets",]
BDSF <- finance[finance$event == "Business Disruption and System Failures",]
IF <- finance[finance$event == "Internal Fraud",]
EF <- finance[finance$event == "External Fraud",]
EPWS <- finance[finance$event == "Employment Practices and Workplace Safety",]
# rerun the codes above for parameters of normal distribution and percentile. 

table(finance$event, finance$settleY)
table(oprisk_data$event, oprisk_data$settleY)



# Banking - Credit Risk: Compile a transition matrix 
# open Credit Risk file 
library(haven)
fund_ratings_13 <- read_dta("~/Desktop/fund_ratings_13.dta")
View(fund_ratings_13)

# explore the data 
table(fund_ratings_13$fyearq)    # fyearq is data reported year 
2005  2006  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016 
1184 29140 30290 29955 30297 30331 29569 29818 30119 29692 17565    98 
table(fund_ratings_13$year)
2006  2007  2008  2009  2010  2011  2012  2013  2014  2015 
30816 30260 29913 30298 30296 29543 29881 30092 29626 17333 
table(fund_ratings_13$splticrm)
table(fund_ratings_13$next_long_rat)

table(fund_ratings_13$year, fund_ratings_13$splticrm)
A    A-    A+    AA   AA-   AA+   AAA     B    B-    B+    BB   BB-   BB+   BBB  BBB-  BBB+
  2006 23016   530   576   313   103   179    28    64   480   310   757   618   803   432  1015   644   744
2007 22761   513   538   277   147   161    29    64   559   288   692   562   793   420   928   667   711
2008 22640   482   524   254   123   185    23    59   553   320   633   537   749   347   932   682   692
2009 23256   503   436   218   101   140    22    43   522   378   539   465   634   328   987   684   648
2010 23285   493   480   217   100   127    19    37   640   346   584   485   604   331   929   791   617
2011 22468   469   493   243    77   125    20    36   655   255   632   501   587   411   939   763   694
2012 22828   412   558   231    50   125    24    32   591   258   662   488   568   447   983   736   719
2013 22973   405   592   230    58   118    24    32   624   248   651   529   576   400  1027   783   700
2014 22373   420   633   222    66   132    20    29   636   256   582   546   634   475   982   790   721
2015 11574   369   495   172    45   110    12    16   416   204   459   445   453   415   765   656   618

CC   CCC  CCC-  CCC+     D    SD
2006     5    41     5   115    34     4
2007    10    33     5    74    23     5
2008    14    35     9    80    28    12
2009    41   106    15   133    82    17
2010    17    34    18    98    39     5
2011    13    42     5    87    23     5
2012     8    55     9    66    25     6
2013     2    36     8    56    15     5
2014     2    17    18    64     4     4
2015     2    15    17    62     8     5
# missing variables 
anyNA(fund_ratings_13$splticrm)    # False - no
anyNA(fund_ratings_13$next_long_rat)    # False - no

vars <- c("datadate", "fyearq", "splticrm", "year", "next_long_rat")
View(fund_ratings_13[vars])

# import a package to build transition matrix 
# package: markovchain 
install.packages("markovchain")
library(markovchain)
mcFit <- markovchainFit(data = vars)

trans.matrix <- function(X, prob=T){
  tt <- table(c(X[,-ncol(X)]), c(X[,-1]))
  if(prob) 
    tt <- tt / rowSums(tt)
  tt
}
trans.matrix(as.matrix(vars))

# strip data by group_by years and create matrices following datadate 
table <- subset(fund_ratings_13, select = c("splticrm", "next_long_rat"))
table <- table[table$splticrm != "",]
table <- table[table$next_long_rat != "",]
anyNA(table)    # FALSE 
output <- trans.matrix(as.matrix(table))
View(trans.matrix(as.matrix(table)))

table1 <- subset(fund_ratings_13, fund_ratings_13$year == "2013", select = vars)
table1 <- table1[table1$splticrm != "",] 
table1 <- table1[table1$next_long_rat != "",]

table2 <- subset(fund_ratings_13, fund_ratings_13$year == "2014", select = vars)
table2 <- table2[table2$splticrm != "",]
table2 <- table2[table2$next_long_rat != "",]
trans.matrix(as.matrix(table1))
trans.matrix(as.matrix(table2))

# remove self-created variables 
rm(vars, mcFit)

install.packages("xlsx")
library("xlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
#write.xlsx(output, "~/Downloads/transition_matrix.xlsx")
#write.xlsx(output, file, sheetName = "transition_matrix", 
#           col.names = TRUE, row.names = TRUE, append = FALSE)
#write.xlsx(output, file = "transition_matrix.xlsx",
#           sheetName = "Sheet1", append = FALSE)
write.csv(output, file = "transition_matrix.csv")



# Calculate Value-at-Risk using historical approach
library(readxl)
GMstk <- read_excel("~/Desktop/GMstk.xlsx")
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
