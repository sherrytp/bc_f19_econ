# ADEC 7430: Big Data Econometrics
# R Lab: Principal Components Analysis

# We perform PCA on the "USArrests" data set

# The rows of the data set contain the 50 states
states <- row.names(USArrests)
states

# The columns of the data set contain the four variables
names(USArrests)

# Briefly examine the data set
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

# Because the variables have vastly different means and variances,
# we must be sure to scale them before performing PCA. Therefore, 
# we standardize them to have mean 0 and standard deviation of 1

# Perform PCA
pr.out <- prcomp(USArrests, scale = TRUE)
names(pr.out)
pr.out$center   # means of the variables used for scaling
pr.out$scale    # standard deviations of variables used for scaling

# Display the rotation matrix providing the principal component loadings
# When we matrix-multiply the X matrix by the rotation matrix, we get the
# coordinates of the data in the rotated coordinate system (principal component scores)
pr.out$rotation

# Get the principal component score vectors
dim(pr.out$x)
head(pr.out$x)

# Let's plot the first two principal components
biplot(pr.out, scale = 0)
# The scale = 0 ensures that the arrows are scaled to represent the loadings
pr.out$rotation = -pr.out$rotation
pr.out$x <- -pr.out$x
biplot(pr.out, scale = 0)

# Get the standard deviation of each principal component
pr.out$sdev

# The variance explained by each principal component
pr.var <- pr.out$sdev^2
pr.var

sum(pr.var)
sum(apply(scale(USArrests), 2, var))

# Compute the proportion of variance explained by each principal component
pve <- pr.var / sum(pr.var)
pve
# The first principal component explains 62% of the variance, and the second
# principal component explains 24.8% of the variance

# Let's plot the PVE of each principal component as well as the cumulative PVE
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained",
     ylim = c(0,1), type = 'b')
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative PVE", 
     ylim = c(0,1), type = 'b')

cumsum(pve)

## Lab 3: NCI60 Data Example
library(ISLR)
nci.labs <- NCI60$labs
nci.data <- NCI60$data
dim(nci.data)  # 64 cancer cell lines and 6830 gene expression measurements

# Let's examine the cancer types for the cell lines
nci.labs[1:4]
table(nci.labs)

# We perform PCA on the data after scaling the variables (genes)
pr.out <- prcomp(nci.data, scale = TRUE)

# Now we plot the first few principal component score vectors to visualize the data
# The color of an observation corresponds to a specific cancer type
Cols <- function(vec){
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

# Now let's plot the principal component score vectors
par(mfrow = c(2,2))
plot(pr.out$x[,1:2], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")
plot(pr.out$x[,c(1,3)], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z3")

plot(nci.data[,1:2], col=Cols(nci.labs), pch=19,xlab="X1",ylab="X2")
plot(nci.data[,c(1,3)], col=Cols(nci.labs), pch=19,xlab="X1",ylab="X3")

# Let's summarize the proportion of variance explained
summary(pr.out)

# Let's plot of the PVE
plot(pr.out)

# Let's plot the PVE of each prinicipal component using a Scree Plot
pve <- 100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow = c(1,2))
plot(pve, type = "o", ylab = "PVE", xlab = "Principal Component", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE", xlab = "Principal Component", col = "brown3")

head(cumsum(pve), 12)