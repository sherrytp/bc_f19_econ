getwd()
setwd("/Users/apple/Documents/R/Forecasting")
getwd()

# Clear the workspace
rm(list = ls()) # Clear environment
gc()            # Clear unused memory
cat("\f")       # Clear the console

print("hello world")
# create a matrix of data and name it 
A <- matrix(c(1,2,3,4,5,6,7,8), nrow = 4, ncol = 2)
B <- matrix(c(1,2,3,4,5,6,7,8), nrow = 4, ncol = 2, byrow = TRUE)

pets <- c("cat", "bunny", "dog")
weight <- c(5,2,30)
feed <- c("yes", "", "no")
run <- c(1, NA, 10)

# create a dataframe 
house.pets <- data.frame(type = pets, weight, feed, run)
### ---- if the data type is a factor, you can change it into a character; factors show those as levels, related to each other 
View(house.pets)
house.pets$type <- as.character(house.pets$type)

#install.packages("rgl")
#install.packages("gold")
library(fpp2)
library(car)
library(rgl)
?Prestige 
prestige2  <- Prestige
rm(prestige2)

# forecasting graphs 
### ---- span can be changed but the values might be invalid 
scatterplot(prestige ~ income|type, boxplots = FALSE, span = 0.75, data = Prestige)
scatterplotMatrix(~ prestige + income + education, span = 0.7, data = Prestige)
# 3D GRAPHING 
scatter3d(prestige ~ income + education, id.n = 3, data = Duncan, warnings = FALSE)

# Inclass Lecture Exercise 
autoplot(melsyd[, "Economy.Class"])
autoplot(elecdaily[, "Temperature"]) + xlab("Week") + ylab("Max temperature")
# change of range of temperature, so a scatterplot would be better 
qplot(time(elecdaily), elecdaily[, "Temperature"]) + xlab("Week") + ylab("Max temperature")

# ggseason plot using Australian diabetic drugs 
ggseasonplot(a10, year.labels = TRUE, year.labels.left = TRUE) + ylab("$ million") + 
  ggtitle("Seasonal plot: antidiabetic drug sales")
ggseasonplot(a10, polar = TRUE) + ylab("$ million")
ggsubseriesplot(a10) + ylab("$ million") + ggtitle("Subseries plot: antidiabetic drug sales")

beer <- window(ausbeer, start = 1992)
autoplot(beer)
ggseasonplot(beer, year.labels = TRUE)
ggsubseriesplot(beer) + ggtitle("Subseries plot: beear consumption")

### ------------ 
### ---- Debug tring 3D 
install.packages("scatterplot3d")  # install
library("scatterplot3d")    # load 
data("iris")
scatterplot3d(iris[, 1:3])

# change the main title and axis labels 
scatterplot3d(iris[,1:3],
              main="3D Scatter Plot",
              xlab = "Sepal Length (cm)",
              ylab = "Sepal Width (cm)",
              zlab = "Petal Length (cm)")
# change the shape and the color of points 
scatterplot3d(iris[, 1:3], pch = 16, color = "steelblue")
# change point shapes by groups 
shape = c(16, 17, 18)
shape <- shape[as.numeric(iris$Species)]
scatterplot3d(iris[, 1:3], pch = shape)
# change point colors by groups 
cls <- c("#99999", "#E69F00", "#56B4E9")
cls <- cls[as.numeric(iris$Species)]
scatterplot3d(iris[,1:3], pch = 16, color=rbc)
##???????? RGB specification 

# remove the box around plot 
scatterplot3d(iris[, 1:3], pch = 16, #color = colors, 
              grid = TRUE, box = FALSE)
# add grids on different facets of plot 
source("http://www.sthda.com/sthda/RDoc/function/addgrids3d.r")
scatterplot3d(iris[, 1:3], pch = 16, grid = FALSE, box = FALSE)
addgrids3d(iris[, 1:3], grid = c("xy", "xz", "yz"))

source('~/hubiC/Documents/R/function/addgrids3d.r')
s3d <- scatterplot3d(iris[, 1:3], pch = "", grid = FALSE, box = FALSE)
addgrids3d(iris[, 1:3], grid = c("xy", "xz", "yz"))
s3d$points3d(iris[, 1:3], pch = 16)
# add bars
scatterplot3d(iris[, 1:3], pch = 16, type = "h", color = cls)
# Custom shapes/colors 
s3d <- scatterplot3d(iris[,1:3], pch = shape, color=cls)
legend("bottom", legend = levels(iris$Species),
       col =  c("#999999", "#E69F00", "#56B4E9"), 
       pch = c(16, 17, 18), 
       inset = -0.25, xpd = TRUE, horiz = TRUE)
# add point labels 
scatterplot3d(iris[, 1:3], pch = 16, color = cls)
text(s3d$xyz.convert(iris[, 1:3]), labels = rownames(iris), 
    cex = 0.7, col = "steelblue")
# 3D scatter plot 
s3d <- scatterplot3d(trees, type = "h", color = "blue", 
                     angle = 55, pch = 16)
my.lm <- lm(trees$Volume ~ trees$Girth + trees$Height)
s3d$plane3d(my.lm)
s3d$points3d(seq(10,20,2), seq(85,60,-5), seq(60,10,-10), 
             col = "red", type = "h", pch = 8)

```
"Output may be on screen using OpenGL, or to various standard 3D file formats including WebGL, PLY, OBJ, STL" ;
"There are two ways in which rgl scenes are normally displayed within R. The older one is in a dedicated window. In Unix-alikes this is an X11 window; it is a native window in Microsoft Windows. On MacOS, the XQuartz system (see http://xquartz.org) needs to be installed to support this."
"The newer way to display a scene is by using WebGL in a browser window or in the Viewer pane in RStudio. To select this, set options(rgl.printRglwidget = TRUE). Each operation that would change the scene will return a value which triggers a new WebGL display when printed."
```
rglwidget()
options(rgl.printRglwidget = TRUE)

# A few exercise explaining the trend of graph showing, like homework 
autoplot(window(elec, start=1980)) +
  ggtitle("Australian electricity production") +
  xlab("Year") + ylab("GWh")
# There is a trend of growing; seasonality happening every year during the same period. 

autoplot(bricksq) +
  ggtitle("Australian clay brick production") +
  xlab("Year") + ylab("million units")
# Trend + cyclical 


# Scatterplot 
autoplot(elecdemand[,c("Demand","Temperature")], facets=TRUE) +
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")

qplot(Temperature, Demand, data=as.data.frame(elecdemand)) +
  ylab("Demand (GW)") + xlab("Temperature (Celsius)")

# lag plots and autocorrelation 
beer2 <- window(ausbeer, start=1992)
gglagplot(beer2)
ggseasonplot(beer2)
# y_t plotted against y_t−k

# White Noise 
pigs2 <- window(pigs, start=1990)
autoplot(pigs2) +
  xlab("Year") + ylab("thousands") +
  ggtitle("Number of pigs slaughtered in Victoria")
ggAcf(pigs2)
# Significant lag1, lag2, lag3 
# These show the series is not a white noise series.

