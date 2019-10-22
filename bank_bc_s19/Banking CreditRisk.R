# Banking - Credit Risk: Compile a transition matrix 

rm(list=ls())
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

fund_ratings_13$equity <- fund_ratings_13$atq - fund_ratings_13$ltq
fund_ratings_13$volasset <- sd(fund_ratings_13$ch_mktval) 
