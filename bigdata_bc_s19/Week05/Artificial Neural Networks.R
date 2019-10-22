# ADEC 7430 Big Data Econometrics

# Artificial Neural Networks

## Example 1: Create a ANN to perform square rooting

library(neuralnet)  # For Neural Network

# Generate 50 random numbers uniformly distributed between 0 and 100
# and store them as a dataframe
traininginput <-  as.data.frame(runif(50, min = 0, max = 100))
trainingoutput <- sqrt(traininginput)

# Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")

# Train the neural network
# Going to have 10 hidden layers
# Threshold is a numeric value specifying the threshold for the partial
# derivatives of the error function as stopping criteria.
net.sqrt <- neuralnet(Output ~ Input, trainingdata, hidden = 10, threshold = 0.01)
print(net.sqrt)

# Plot the neural network
plot(net.sqrt)

# Test the neural network on some training data
testdata <- as.data.frame((1:10)^2) # generate some squared numbers
net.results <- compute(net.sqrt, testdata) # run them through the neural network

# Let's see what properties net.sqrt has
ls(net.results)

# Let's see the results
print(net.results$net.result)

# Let's display a better version of the results
cleanoutput <- cbind(testdata,sqrt(testdata), as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
print(cleanoutput)


# Example 2: Create a ANN for prediction

# We give a brief example of regression with neural networks and comparison with
# multivariate linear regression. The data set is housing data for 506 census tracts of
# Boston from the 1970 census. The goal is to predict median value of owner-occupied homes.

# Load the data and inspect the range (which is 1 - 50)
library(mlbench)
data(BostonHousing)
summary(BostonHousing$medv)

# Build the multiple linear regression model
lm.fit <- lm(medv ~ ., data = BostonHousing)
lm.predict <- predict(lm.fit)

# Calculate the MSE and plot
mean((lm.predict - BostonHousing$medv)^2) # MSE = 21.89483
par(mfrow = c(2,1))
plot(BostonHousing$medv, lm.predict, main = "Linear Regression Predictions vs Actual (MSE = 21.9)", 
     xlab = "Actual", ylab = "Predictions", pch = 19, col = "brown")

# Build the feed-forward ANN (w/ one hidden layer)
library(nnet)        # For Neural Network
nnet.fit <- nnet(medv/50 ~ ., data = BostonHousing, size = 2) # scale inputs: divide by 50 to get 0-1 range
nnet.predict <- predict(nnet.fit)*50 # multiply 50 to restore original scale

# Calculate the MSE and plot 
mean((nnet.predict - BostonHousing$medv)^2) # MSE = 16.56974
plot(BostonHousing$medv, nnet.predict, main = "Artificial Neural Network Predictions vs Actual (MSE = 16.6)",
     xlab = "Actual", ylab = "Predictions", pch = 19, col = "blue")

# Next, we use the function train() from the package caret to optimize the ANN 
# hyperparameters decay and size, Also, caret performs resampling to give a better 
# estimate of the error. We scale linear regression by the same value, so the error 
# statistics are directly comparable.

library(mlbench)
data(BostonHousing)
library(caret)

# Optimize the ANN hyperpameters and print the results
mygrid <- expand.grid(.decay = c(0.5, 0.1), .size = c(4, 5, 6))
nnet.fit2 <- train(medv/50 ~ ., data = BostonHousing, method = "nnet", maxit = 1000, 
                 tuneGrid = mygrid, trace = FALSE)
print(nnet.fit2)

# Scale the linear regression and print the results
lm.fit2 <- train(medv/50 ~ ., data = BostonHousing, method = "lm") 
print(lm.fit2)


# Example 3: Create a ANN for classification

# Predict the identification of Iris plant Species on the basis of plant 
# attribute measurements: Sepal.Length, Sepal.Width, Petal.Length, Petal.Width

library(RSNNS)

# Load and store the 'iris' data
data(iris)

# Generate a sample from the 'iris' data set
irisSample <- iris[sample(1:nrow(iris), length(1:nrow(iris))), 1:ncol(iris)]
irisValues <- irisSample[, 1:4]
head(irisValues)
irisTargets <- irisSample[, 5]
head(irisTargets)

# Generate a binary matrix from an integer-valued input vector representing class labels
irisDecTargets <- decodeClassLabels(irisTargets)
head(irisDecTargets)

# Split the data into the training and testing set, and then normalize
irisSample <- splitForTrainingAndTest(irisValues, irisDecTargets, ratio = 0.15)
irisSample <- normTrainingAndTestSet(irisSample)

# Train the Neural Network (Multi-Layer Perceptron)
nn3 <- mlp(irisSample$inputsTrain, irisSample$targetsTrain, size = 2, learnFuncParams = 0.1, maxit = 100, 
	inputsTest = irisSample$inputsTest, targetsTest = irisSample$targetsTest)
print(nn3)

# Predict using the testing data
testPred6 <- predict(nn3, irisSample$inputsTest)

# Calculate the Confusion Matrices for the Training and Testing Sets
confusionMatrix(irisSample$targetsTrain, fitted.values(nn3))
confusionMatrix(irisSample$targetsTest, testPred6)

# Calculate the Weights of the Newly Trained Network
weightMatrix(nn3)

# Plot the Iterative Error of both training (black) and test (red) error
# This shows hows the Number of Iterations Affects the Weighted SSE
plotIterativeError(nn3, main = "# of Iterations vs. Weighted SSE")
legend(80, 80, legend = c("Test Set", "Training Set"), col = c("red", "black"), pch = 17)

# See how changing the Learning Rate Affects the Average Test Error
err <- vector(mode = "numeric", length = 10)
learnRate = seq(0.1, 1, length.out = 10)
for (i in 10){
	fit <- mlp(irisSample$inputsTrain, irisSample$targetsTrain, size = 2, learnFuncParams = learnRate[i], maxit = 50, 
	  inputsTest = irisSample$inputsTest, targetsTest = irisSample$targetsTest)
	err[i] <- mean(fit$IterativeTestError)
}

# Plot the Effect of Learning Rate vs. Average Iterative Test Error
plot(learnRate, err, xlab = "Learning Rate", ylab = "Average Iterative Test Error",
	main = "Learning Rate vs. Average Test Error", type = "l", col = "brown")