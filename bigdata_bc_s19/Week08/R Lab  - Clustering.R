# ADEC 7430: Big Data Econometrics
# R Lab: Clustering

## 10.5.1 K-Means Clustering

# We begin with a simple simulated example
set.seed(2)
x <- matrix(rnorm(50*2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4

# We now perform K-means clustering with K = 2
km.out <- kmeans(x, 2, nstart = 20)

# The cluster assignment are contained in the following
km.out$cluster

# Let's plot the data, with each observation colored according to its cluster assignment
plot(x, col = (km.out$cluster + 1), main = "K-Means Clustering Results with K=2", 
     xlab = "", ylab = "", pch = 20, cex = 2)

# Perform K-means clustering with K = 3 and plot
km.out <- kmeans(x, 3, nstart = 20)
plot(x, col = (km.out$cluster + 1), main = "K-Means Clustering Results with K=3", 
     xlab = "", ylab = "", pch = 20, cex = 2)

# Now perform K-means clustering with K=3
set.seed(4)
km.out <- kmeans(x, 3, nstart = 20)
km.out

# Compare nstart = 1 to nstart = 20 to nstart = 50
set.seed(3)
km.out <- kmeans(x, 3, nstart = 1)
km.out$tot.withinss  # total within cluster sum of squares
km.out <- kmeans(x, 3, nstart = 20)
km.out$tot.withinss
km.out <- kmeans(x, 3, nstart = 50)
km.out$tot.withinss


## 10.5.2 Hierarchical Clustering

# Perform complete, single and average linkage clustering
hc.complete <- hclust(dist(x), method = "complete")
hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")

# Plot the dendrograms obtained
par(mfrow = c(1,3))
plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", cex = .9)
plot(hc.average, main = "Average Linkage", xlab = "", sub = "", cex = .9)
plot(hc.single, main = "Single Linkage", xlab = "", sub = "", cex = .9)

# Plots of the hierarchical clustering results
par(mfrow = c(1,1))
plot(x, col=(cutree(hc.complete, 2)+1), main="2-cluster Hierarchical Results for Complete Linkage", 
     xlab="", ylab="", pch=20, cex=2)
plot(x, col=(cutree(hc.average, 2)+1), main="2-cluster Hierarchical Results for Average Linkage", 
     xlab="", ylab="", pch=20, cex=2)
plot(x, col=(cutree(hc.single, 2)+1), main="2-cluster Hierarchical Results for Single Linkage", 
     xlab="", ylab="", pch=20, cex=2)
plot(x, col=(cutree(hc.single, 4)+1), main="4-cluster Hierarchical Results for Single Linkage", 
     xlab="", ylab="", pch=20, cex=2)

# Determine the cluster labels for each observations associated with a given cut of the dendrogram
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)

# Can scale the variables before performing hierarchical clustering
par(mfrow = c(1,1))
xsc <- scale(x)
plot(hclust(dist(xsc), method = "complete"), main = "Hierarchical Clustering with Scaled Features")

# Use correlation-based distance for a 3-D data set
x <- matrix(rnorm(30*3), ncol = 3)
dd <- as.dist(1 - cor(t(x)))
plot(hclust(dd, method = "complete"), main = "Complete Linkage with Correlaton-Based Distance", 
     xlab = "", sub = "")


## Lab 3: NCI60 Data Example
library(ISLR)
nci.labs <- NCI60$labs
nci.data <- NCI60$data
dim(nci.data)  # 64 cancer cell lines and 6830 gene expression measurements

# Let's examine the cancer types for the cell lines
nci.labs[1:4]
table(nci.labs)

# We first standardize the variables so that each gene is on the same scale
sd.data <- scale(nci.data)

# We now proceed to hierarchically cluster the cell lines, with the goal of finding out whether or not
# the observations cluster into distinct types of cancer
par(mfrow = c(3,1))
data.dist <- dist(sd.data)
plot(hclust(data.dist), labels = nci.labs, main = "Complete Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "average"), labels = nci.labs, main = "Average Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "single"), labels = nci.labs, main = "Single Linkage", xlab = "", sub = "", ylab = "")

# We cut the dendrogram at the height yielding a particular number of clusters
hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)

# Now let's plot the dendrogram that produces these four clusters
par(mfrow = c(1,1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red")
hc.out

# How do these results compare to performing K-means clustering with K = 4?
set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)

table(km.clusters, nci.labs)

# Let's perform hierarchical clustering on the first few principal components score vectors
pr.out <- prcomp(nci.data, scale = TRUE)
hc.out <- hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels = nci.labs, main = "Hier. Clust. on First Five PCA Score Vectors")
table(cutree(hc.out, 4), nci.labs)