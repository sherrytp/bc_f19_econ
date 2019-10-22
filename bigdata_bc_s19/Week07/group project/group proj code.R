charity <- read.csv("D:/study in BC/Big data/group project/charity.csv")
library(psych)
describe(charity)
colnames(charity)[sapply(charity,anyNA)]
charity.train <- charity[charity$part=="train",]
library(ggplot2)
library(tidyr)
library(purrr)
charity.train[,-1] %>%
  keep(is.numeric) %>%
  gather()  %>%
  ggplot(aes(value)) + 
  facet_wrap(~ key, scales = "free") + 
  geom_density()

data.train <- charity.train
x.train <- data.train[,2:21]
y.train.donr <- data.train[,22] # donr
y.train.damt <- data.train[y.train.donr==1,23] # damt for observations with donr=1
# y.train.damt <- data.train["donr"==1,23] # damt for observations with donr=1
n.train.donr <- length(y.train.donr) # 3984
n.train.damt <- length(y.train.damt) # 1995

data.valid <- charity[charity$part=="valid",]
x.valid <- data.valid[,2:21]
y.valid.donr <- data.valid[,22] # donr
y.valid.damt <- data.valid[y.valid.donr==1,23] # damt for observations with donr=1
n.valid.donr <- length(y.valid.donr) # 2018
n.valid.damt <- length(y.valid.damt) # 999

data.test <- charity[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:21]


# standardize to have zero mean and unit sd

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)

# performing the standardization at this step

x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) 
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.donr <- data.frame(x.train.std, donr=y.train.donr) # to classify donr
data.train.std.damt <- data.frame(x.train.std[y.train.donr==1,], damt=y.train.damt) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.donr <- data.frame(x.valid.std, donr=y.valid.donr) # to classify donr
data.valid.std.damt <- data.frame(x.valid.std[y.valid.donr==1,], damt=y.valid.damt) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)


## Classification Model
library(MASS)

model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c) # include additional terms on the fly using I()

# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model

post.valid.lda1 <- predict(model.lda1, data.valid.std.donr)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit
# 1329.0 11624.5

cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, c.valid)