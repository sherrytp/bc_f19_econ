# ADEC 7430 - Big Data Econometrics
# R Lab #1: Introduction to R (pp. 42 - 51)

# 2.3.1 Basic Commands

x <- c(1, 3, 2, 5)
x
x = c(1, 6, 2)
x
y <- c(1, 4, 3)

length(x)
length(y)
x+y

ls()
rm(x,y)
ls()
rm(list=ls())

?matrix
x <- matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)
x
x <- matrix(c(1,2,3,4), 2,2)
matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2,  byrow = T)
sqrt(x)
x^2

x <- rnorm(50)
y = x + rnorm(50, mean = 50, sd = .1)
cor(x, y)

set.seed(1303)
rnorm(50)

set.seed(3)
y = rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

# 2.3.2 Graphics

x <- rnorm(100)
y <- rnorm(100)
plot(x, y)
plot(x, y, xlab = "this is the x-axis", ylab = "this is the y-axis",
     main = "Plot of X vs Y", col = "blue", pch = 19)

pdf("Figure.pdf")
plot(x, y, col = "green")
dev.off()

x <- seq(1, 10)
x <- 1:10
x <- seq(-pi, pi, length = 50)
x
y <- x
f <- outer(x, y, function(x, y)cos(y)/(1+x^2))
contour(x, y, f)
contour(x, y, f, nlevels = 45, add = T)
fa <- (f-t(f))/2
contour(x, y, fa, nlevels = 15)

image(x, y, fa)
persp(x, y, fa)
persp(x, y, fa, theta = 30)
persp(x, y, fa, theta = 30, phi = 20)
persp(x, y, fa, theta = 30, phi = 70)
persp(x, y, fa, theta = 30, phi = 40)

# 2.3.3 Indexing Data

A <- matrix(1:16, 4, 4)
A
A[2, 3]
A[c(1 ,3) ,c(2 ,4)]
A[1:3, 2:4]
A[1:2,]
A[, 1:2]
A[1,]
A[-c(1,3),]
A[-c(1 ,3) ,-c(1 ,3 ,4)]
dim(A)

# 2.3.4 Loading Data

Auto <- read.table(file.choose(), header = T, na.strings = "?")
str(Auto)
fix(Auto)
Auto <- read.csv("Auto.csv", header = T, na.strings = "?")
fix(Auto)
dim(Auto)
Auto[1:4,]
head(Auto)
head(Auto, 20)
Auto <- na.omit(Auto)
dim(Auto)
names(Auto)

# 2.3.5 Additional Graphical and Numerical Summaries

plot(cylinders, mpg)
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)

cylinders <- as.factor(cylinders)
plot(cylinders, mpg)
plot(cylinders, mpg, col = "red")
plot(cylinders, mpg, col = "red", varwidth = T)
plot(cylinders, mpg, col = "red", varwidth = T, horizontal = T)
plot(cylinders, mpg, col = "red", varwidth = T, xlab = "cylinders", ylab = "MPG")

hist(mpg)
hist(mpg, col = 2)
hist(mpg, col = 2, breaks = 15)

pairs(Auto)
pairs(~mpg + displacement + horsepower + weight + acceleration, Auto)

plot(horsepower, mpg)
identify(horsepower, mpg, name)

summary(Auto)
summary(mpg)
library(psych)
install.packages("psych")
describe(Auto)

# R Lab #1 - Solutions

# Q1
set.seed(3)
y = rnorm(100)
summary(y)
median(y)

# Q2
?plot
# sub: a sub title for the plot: see title.

# Q3
X <- matrix(1:16, 4, 4)
X[, 2]

# Q4
str(Auto)

# Q5
attach(Auto)
cylinders=as.factor(cylinders)
plot(cylinders, mpg)
# From the resulting boxplots the medians are ordered 4, 5, 3, 6, 8.

# Q6
hist(mpg)
# The resulting histogram looks slightly skewed with a long right tail.

# Q7
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
# Of the four scatterplots in the top row (with mpg on the vertical axis), only the scatterplot of 
# acceleration versus mpg in the top right doesn't show much evidence of an association.

# Q8
plot(horsepower,mpg)
identify(horsepower,mpg,name)
# The car that "sticks out" is observation #334, which is a Datsun 280-zx.