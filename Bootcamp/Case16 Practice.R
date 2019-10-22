# March 17, 2019 
# Peng Tian R Practice 
# ----------- 16. Case Study: Working Through a HW Problem -----------------------

# read file 
rm(list=ls())
engine <- read.csv("/Users/apple/Downloads/table_7_3.csv", sep = ",", header = TRUE)
names(engine)
summary(engine)

# look at the distribution, skewed 
# b/c the spread between Q3 and max is 5 times the spread between min and Q1
qqnorm(engine$co, main = "Carbon Monoxide")   # normal QQ plot 
qqline(engine$co)
boxplot(engine$co, main = "Carbon Monoxide")
hist(engine$co, main = "Carbon Monoxide")
qqnorm(engine$co, main = "Carbon Monoxide")
qqline(engine$co)

# tranform into logarithm
lengine <- log(engine$co)
boxplot(lengine, main = "Carbon Monoxide")
hist(lengine, main = "Carbon Monoxide")
qqnorm(lengine, main = "QQ plot for the Log of the Carbon Monoxide")
qqline(lengine)

# find the confidence interval for the carbon monoxide data 
# we will t-distribution with parameters 
m <- mean(lengine)
s <- sd(lengine)
n <- length(lengine)

# find the standard error 
se <- s/sqrt(n)
se

# find the margin of error based on confidene level of 95% 
error <- se*qt(0.975, df = n-1)
left <- m - error
right <- m + error
left
right 
# 95% confidence interval is between 1.71 and 2.06

# find the interval for original data by undoing log 
exp(left)
exp(right)
# 95% confidence interval for the carbon monoxide is between 5.53 and 7.83

# test of significance - two-sided hypothesis test 
# suppose null hypothesis of a mean level of 5.4 
lNull <- log(5.4) - error
rNull <- log(5.4) + error
lNull
rNull

# Alternatively, calculate the actual p-value 
2*(1-pt(m-log(5.4))/se, df = n-1)

# one sample t-test
t.test(lengine, mu = log(5.4), alternative = "two.sided")  # help(t.test)

# find the power of the test 
tLeft <- (lNull - log(7)/(s/sqrt(n)))
tRight <- (rNull - log(7)/(s/sqrt(n)))
p <- pt(tRight, df = n-1) - pt(tLeft, df = n-1) 
p 
1-p

t <- qt(0.975, df = n-1)
shift <- (log(5.4) - log(7)/(s/sqrt(n)))
pt(t, df = n-1, ncp = shift) - pt(-t, df = n-1, ncp = shift)

1-pt(t, df = n-1, ncp = shift) - pt(-t, df = n-1, ncp = shift)

# run the last two-sample t-test approach 
power.t.test(n = n, delta = log(7) - log(5.4), sd = s, sig.level = 0.05)





# ----------- Forecasting Homework Practice Assignment1 --------------
# Using RMarkdown 

# Chapter5 Exercise
# 5.1 Daily Electricty Demand 
daily20 <- head(elecdaily,20)
# a
plot(daily20)
# b 
daily20 %>%
  as.data.frame() %>% 
  ggplot(aes(x=Temperature, y=Demand)) +
  geom_point() + 
  geom_smooth(method="lm", se=FALSE)
# c
fit <- tslm(Demand ~ Temperature, data=daily20)
checkresiduals(fit)
summary(fit)
# d
forecast(fit, newdata=data.frame(Temperature=c(15,35)))
# e
elecdaily%>%
  as.data.frame()%>%
  ggplot(aes(x=Temperature,y=Demand))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)

#5.2 Olympic Games
# a 
data <- mens400
autoplot(mens400)
# b
autoplot(mens400)+
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
# c
fit <- tslm(mens400~trend)
print(fit)
checkresiduals(fit)
# d
#d
rwf(mens400, drift=TRUE, h=1)

#5.3 Easter  
easter(ausbeer)

#5.5 Fancy
# a
plot(fancy)
# b
fancy1 <- as.data.frame(fancy)
fancy2 <- as.numeric(unlist(fancy1))
p <- hist(fancy2,col='blue')
# c
fancy1$logx <- log(fancy1$x)
fit <- tslm(BoxCox(fancy,0)~trend+season)
summary(fit)
print(fit)
checkresiduals(fit)
# e
box <-residuals(fit)

box1<-data.frame(box,Month=rep(1:12,7))
boxplot(box~Month,data=box1)
checkresiduals(fit)
# g
bg <- bgtest(fit, order=1, data=fancy2)    # cannot find the function 
print(bg)
# h
rwf(fancy, drift=TRUE, h=36)

#5.6 Gasoline
#a 
weeklygas <- window(gasoline, end=2005)
length(weeklygas)
weeklygas1<- tslm(weeklygas~ trend + fourier(weeklygas, K=26))

autoplot(weeklygas, series= 'Observed Gasoline')+autolayer(fitted(weeklygas1), series='Fitted Value') +
  ggtitle("K=26")+xlab('Year')+ylab ('Gasoline')

#K=9
weeklygas2<- tslm(weeklygas~ trend + fourier(weeklygas, K=9))

autoplot(weeklygas, series= 'Observed Gasoline')+autolayer(fitted(weeklygas2), series='Fitted Value') +
  ggtitle("K=9")+xlab('Year')+ylab ('Gasoline')

#K=4
weeklygas3<- tslm(weeklygas~ trend + fourier(weeklygas, K=4))

autoplot(weeklygas, series= 'Observed Gasoline')+autolayer(fitted(weeklygas3), series='Fitted Value') +
  ggtitle("K=4")+xlab('Year')+ylab ('Gasoline')

#b
CV(weeklygas1)
CV(weeklygas2)
CV(weeklygas3)
#c
checkresiduals(weeklygas2)
#d

wg<-forecast(weeklygas2, newdata=data.frame(fourier(weeklygas,9,52)))

weeklygasoline<-window(gasoline, start=2005, end=2006)

autoplot(weeklygasoline, series=' Observed Gasoline') + autolayer(wg$mean, series='Fitted Value')+
  ggtitle("Comparing Prediction and Actual")+xlab('Year')+ylab('Gasoline')
#5.7
#a
autoplot(huron)+ xlab('Year')+ ggtitle('Water Level of Lake Huron')
#b
#Linear Reg
fit7<-tslm(huron~trend)
summary(fit7)
#Piecewise Linear Trend
t <- time(huron)
t1 <- ts(pmax(0, t - 1915), start = 1875)
fitpw <- tslm(huron ~ t + t1)
summary(fitpw)
#c
forecast(fit7, h=8)

#Chapter 6 Exercise
#6.1 Plastic Manufacturers
#2a
autoplot(plastics)+ ggtitle('The Monthly Sales(in thousands) of Product A')
#2b
plastics %>% decompose(type="multiplicative") %>% 
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition 
          of plastics")
#2d

multi<-decompose(plastics, type="multiplicative")

seasonal_adj<-multi$x/multi$seasonal
autoplot(seasonal_adj)+ ylab('Seasonally Adjusted Data')
#2e
pl<-plastics
pl[32]<-pl[32]+500

multi1<-decompose(pl, type="multiplicative")
seasonal_adj1<-multi1$x/multi1$seasonal
autoplot(seasonal_adj1)+ ggtitle('Seasonally Adjusted Data(Outlier Near The Middle')
#2f
pl1<-plastics
pl1[57]<-pl[57]+500

multi2<-decompose(pl1, type="multiplicative")
seasonal_adj2<-multi2$x/multi1$seasonal
autoplot(seasonal_adj2)+ ggtitle('Seasonally Adjusted Data(Outlier Near The End')
#6.3 Decomposition
retaildata<- readxl::read_xlsx("retail.xlsx",skip=1)

myts <- ts(retaildata[,"A3349873A"],
           frequency=12, start=c(1982,4))

myts %>% seas(x11="") %>%
  autoplot() +
  ggtitle("X11 decomposition of Retail Data")

#6.5 Monthly Canadian Gas Production
#5a
autoplot(cangas)
ggsubseriesplot(cangas)
ggseasonplot(cangas)
#5b
cangas %>%
  stl(t.window=46, s.window="periodic", robust=TRUE) %>%
  autoplot()+ ggtitle('STL decomposition of Monthly Canadian Gas Production 1.0 ')
cangas %>%
  stl(t.window=46, s.window=99, robust=TRUE) %>%
  autoplot()+ ggtitle('STL decomposition of Monthly Canadian Gas Production 2.0')
cangas %>%
  stl(t.window=46, s.window=30, robust=TRUE) %>%
  autoplot()+ ggtitle('STL decomposition of Monthly Canadian Gas Production 3.0')
cangas %>%
  stl(t.window=46, s.window=7, robust=TRUE) %>%
  autoplot()+ ggtitle('STL decomposition of Monthly Canadian Gas Production 4.0')
#5c
cangas %>% seas() %>%
  autoplot() +
  ggtitle("SEATS decomposition of Monthly Canadian Gas Production ")
cangas %>% seas(x11="") %>%
  autoplot() +
  ggtitle("X11 Monthly Canadian Gas Production")

#6.6 Australian Quarterly Clay Brick Production
#6a
bricksq %>%
  stl(t.window=39, s.window="periodic", robust=TRUE) %>%
  autoplot()+ 
  ggtitle('STL decomposition of Australian Quarterly Clay Brick Production(Fixed Seasonality)')

bricksq %>%
  stl(t.window=39, s.window=7, robust=TRUE) %>%
  autoplot()+ 
  ggtitle('STL decomposition of Australian Quarterly Clay Brick Production(Changing Seasonality)')
#6b
bricksq %>%
  stl(t.window=39, s.window="periodic", robust=FALSE) %>% seasadj() %>%
  autoplot()+
  ggtitle('STL decomposition of Australian Quarterly Clay Brick Production(Seasonally Adjusted)')
#6c
bsq <- stl(bricksq, t.window=39, s.window="periodic", robust=FALSE)
bsq %>% seasadj() %>% naive() %>% autoplot() +
  ggtitle("Naive forecasts of seasonally adjusted data") + 
  xlab('Year')+ ylab('Clay Brick Production')
#6d
rwf(stlf(bricksq,method='naive'),h=8)
#6e
checkresiduals(stlf(bricksq,method='naive'))
#6f
bsq1 <- stl(bricksq, t.window=39, s.window="periodic", robust=TRUE)
bsq1 %>% seasadj() %>% naive() %>% autoplot() +
  ggtitle("Robust STL decomposition of seasonally adjusted data") +
  xlab('Year')+ ylab('Clay Brick Production')
#6g
bsqtrain<-window(bricksq, end=c(1991,4))
bsqtest<-as.numeric(window(bricksq,start= c(1992,1), end= c(1994,3)))

bsqna<-snaive(bsqtrain)

bsqnaf<-rwf(bsqna, h=11)
navr <- bsqtest- bsqnaf$mean


bsqstlf<-stlf(bsqtrain)
bsqstlff<-rwf(bsqstlf, h=11)
stlfr<-bsqtest-bsqstlff$mean


autoplot(navr,series='Naive Method') + autolayer(stlfr, series='STLF')+ 
  ggtitle('Compare Forecasts')+
  xlab('Year')

#6.7 Writing Series
plot(writing)
wrna <- stlf(writing, method='naive')
wrrwf <- stlf(writing, method='rwdrift')

wrnar<-as.numeric(residuals(wrna))
wrrwfr<-as.numeric(residuals(wrrwf))

wrnar<-na.omit(wrnar)
wrrwfr<-na.omit(wrrwfr)

rmsena<-sqrt(sum((wrnar)^2)/120)
rmserwf<-sqrt(sum((wrrwfr)^2)/120)

rmsena
rmserwf

#6.8 Fancy Series
fancyts=ts(fancy,frequency=12)
plot(fancyts)
ffcna <- stlf(writing, method='naive')
ffcdrf <- stlf(writing, method='rwdrift')
ffclog<- stlf(writing, method='rwdrift', lambda=0)

ffcna
ffcdrf

resnaive<-as.numeric(residuals(ffcna))
resdrift<-as.numeric(residuals(ffcdrf))
reslog<-as.numeric(residuals(ffclog, type="response"))

resnaive
resdrift
reslog

resnaive<-na.omit(resnaive)
resdrift<-na.omit(resdrift)
reslog<-na.omit(reslog)

RMSEnaive<-sqrt(sum((resnaive)^2)/119)
RMSEdrift<-sqrt(sum((resdrift)^2)/119)
RMSElog<-sqrt(sum((reslog)^2)/119)

RMSEnaive
RMSEdrift
RMSElog
# ------------------------- end 

# ======= AT&T Data File 
y <- ts(y, frequency = 12)[,2]
y <- y[-c(1:12)]
y <- ts(y, frequency = 12)
autoplot(y)+
  ylab("Monthly returns for AT&T") + xlab("year")

# Simple Exponential Smoothing
fc1 <- ses(y, h =12)
autoplot(fc1) +
  autolayer(fitted(fc1), series="Fitted") +
  ylab("Monthly returns for AT&T") + xlab("Year")
summary(fc1)

# Holt-Winters’ seasonal method
fit1 <- hw(y,seasonal="additive")
autoplot(y) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  xlab("Year") +
  ylab("Monthly returns for AT&T") +
  guides(colour=guide_legend(title="Forecast"))
summary(fit1)

# Damped trend method
fc2 <- hw(y,damped = TRUE, seasonal = "additive", h=12)
autoplot(y) +
  autolayer(fc2, series="HW additive damped", PI=FALSE)+
  guides(colour=guide_legend(title="Monthly forecasts"))

fc3 <- ets(y,damped = TRUE)
summary(fc3)

# additive decompose 
y1 %>% decompose(type= "additive") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical additive decomposition
          of Scholes index for NYSE")

y1 %>% decompose(type= "additive") -> fit1
t.add <- na.omit(as.numeric(trendcycle(fit1)))
s.add <- na.omit(as.numeric(seasonal(fit1)))
y11 <- y1[-c(95:100)]
y11 <- y11[-c(1:6)]
r.add <- y11-t.add-s.add

var(r.add)

# multiplicative decompose 
y1 %>% decompose(type= "multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
          of Scholes index for NYSE")

y1 %>% decompose(type= "multiplicative") -> fit2
t.mult <- na.omit(as.numeric(trendcycle(fit2)))
s.mult <- na.omit(as.numeric(seasonal(fit2)))
y11 <- y1[-c(95:100)]
y11 <- y11[-c(1:6)]
r.mult <- y11-s.mult-t.mult

var(r.mult)

