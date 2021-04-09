##---------------------------------------------------
##---------------------------------------------------
##----- Intro to R and RStudio
##---------------------------------------------------
##---------------------------------------------------


#-------------------------------------
#----- Step 1: Basics of R
#-------------------------------------

  # Using R as calculator
  1+1
	(3 + (5 * (2 ^ 2))) # hard to read
  3 + 5 * 2 ^ 2       # clear, if you remember the rules
  3 + 5 * (2 ^ 2)     # if you forget some rules, this might help
  
  exp()
  help("exp") # Equivalent to pressing F1
  help("exponent")
  ??exponent

  # Assignment
  x <- 1.25
  y = x # Should not use "=" in assignment
  x <- 2.75
  
  x <- 1
  x <- x + 1
  x <- exp(x)
  
  x <- z + 1 # Can't assign values of non-existing objects 
  
  really.long.variable.name.super.big.long <- 5
  
  .variable.name <- 6
  5variable.name <- 10
  'variable with a space' <- 1

  # Vectorized operations
  x <- 5:10
  x
  x[3]
  exp(x[3])
  exp(x)
  y <- cbind(3:5, 6:8, 8:11)
  y <- cbind(3:5, 6:8, 9:11)
  y
  y[2]
  y[8]
  y[2,1]
  y[1, ]
  y[, 1]
  exp(y)
  mean(y)
  mean(y[1, ])

  # Clear the environment
  ls()
  rm(y)
  list <- ls()
  list
  rm(list)
  rm(list = ls()) # Clears the entire environment


#-------------------------------------
#----- Step 2: Basics of data structures
#-------------------------------------

  # Vectors
  x <- c(1, 2, 3)
  str(x)
  class(x)
  typeof(x)
  x <- c(1L, 2L, 3L)
  str(x)
  class(x)
  typeof(x)
  x <- c(1, 2, 3)
  class(x)
  x <- as.integer(x)
  class(x)

  # Matrices
  y <- cbind(x, x, x, x, 2L*x)
  str(y)
  class(y)
  typeof(y)

  # Lists
  z <- list(1:5, "a", c(TRUE, FALSE), 1+4i)
  z
  str(z)
  class(z)
  typeof(z)
  z[1]
  str(z[1])
  z[3]
  z[1,2]
  z[[1]]
  str(z[[1]])
  z[3][2]
  z[[3]][2]

  # Data Frames
  options(stringsAsFactors = FALSE) # Run this once
  cats <- data.frame(coat = c("grey", "black", "cinnamon")
                     , weight = c(2.1, 5.0,3.2)
                     , likes_string = c(1L, 0L, 1L)
                     )
  cats
  View(cats)
  str(cats)
  class(cats)
  typeof(cats)
  
  cats[1] # Not recommended
  str(cats[1])
  cats[1, ]
  cats[, 1]
  str(cats[, 1])
  cats[1,2]
  cats[2, 1]

  # Working with numerical variables
  cats$coat
  View(cats$coat)
  str(cats$coat)
  
  cats$weight + 2
  
  cats$weight <- cats$weight + 2
  mean(cats$weight)
  
  typeof(cats$weight)
  typeof(cats$likes_string)


  # Working with strings/characters
  typeof(cats$coat)
  cats$weight + cats$coat
  paste("My cat is", cats$coat)
  paste("My cat is", cats$coat, sep = "a")
  paste("My cat is", cats$coat, sep = " a ")
  paste("My cat is a", cats$coat, "one")

  # Working with categorical (factor) variables
  typeof(cats$coat)
  mean(cats$coat)
  summary(cats)
  summary(cats$coat)
  
  cats$coat.new <- as.factor(cats$coat)
  str(cats$coat.new)
  typeof(cats$coat.new)
  mean(cats$coat.new)
  summary(cats$coat.new)

  # Coercion examples
  x <- c(TRUE, FALSE)
  typeof(x)
  y <- c(1L, 0L)
  typeof(y)
  z <- c(x, y)
  View(z)
  typeof(z)
  z <- c(z, 1)
  typeof(z)
  
  x <- c("Yes", "No")
  typeof(x)
  y <- c(1, 1)
  typeof(y)
  z <- c(x, y)
  z
  typeof(z)

#-------------------------------------
#----- Step 3: loops
#-------------------------------------
  
  y <- cbind(1:5, 6:10)
  y
  mean(y)

  # How can we get means for each column in y?
  dim(y)
  nrow(y)
  ncol(y)
  # Loop template:
  # for (index in start:finish) {do stuff}
  for (Anatoly in 1:ncol(y)) {
    mean(y[, Anatoly])
  }
  print("Hello world")
  print(4)
  for (i in 1:ncol(y)) {
    print(mean(y[, i]))
  }

  # What if we want to store the results somewhere?
  rep(1, 5)
  means.y <- rep(NA, ncol(y))
  means.y
  for (i in 1:ncol(y)) {
    means.y[i] <- mean(y[, i])
  }
  means.y
  
#-------------------------------------
#----- Step 4: conditions and logical operators 
#-------------------------------------
  
  # R logical operators:
  # ! (NOT)
  # == (EQUAL)
  # & (AND)
  # | (OR)
  ! 2 == 4 
  2 == 4 & 3 == 5
  ! 2 == 4 & 3 == 5
  ! (2 == 4 & 3 == 5)  # Here NOT is applied to intersection of two EQUALs
  2 == 4 | 3 < 5
  
  3 == 3.0000000001
  3 == 3.000000000000000000001
  
  # If/else statements
  if (2 == 4) print("2 equal 4")
  # Same as
  if (2 == 4) {
    print("2 equal 4")
  }
  # But if you have more than 1 line of code, you need {}, so it's best to use them anyways

  # You can add an else clause
  if (2 == 4) {
    print("2 equal 4")
  } else {  # Note that else has to be on the same line as closing }
    print("2 not equal 4")
  }
  
  # One can combine loops with if
  dim(means.y)
  nrow(means.y)
  length(means.y)
  for (i in 1:length(means.y)) {
    if (means.y[i] > 5) {
      text <- paste("Mean of column", i, "is", means.y[i], ", which is larger than 5")
    } else {
      text <- paste("Mean of column", i, "is", means.y[i], ", which is smaller or equal to 5")
    }
    print(text)
  }
  
  # %in% operator
  2 %in% c(1, 3, 5)
  2 %in% c(1, 2, 5)
  c(1, 2) %in% c(1, 3, 5)
  c(1, 2) %in% c(1, 2, 5)
  c(1, 2) %in% c(1, 1, 1, 3, 3, 3, 5, 5, 5)
  c(1, 2)[c(TRUE, FALSE)]
  c(1, 2)[c(1, 2) %in% c(1, 3, 5)]
  
  # which()
  x <- c(1,3,5,7,9)
  y <- c(5:10)
  x %in% y
  which(x %in% y)
  x[which(x %in% y)] 
  
  # which() is usefull when you expect only a few items to be TRUE
  x <- 8:10000
  x %in% 1:10 # returns too much info
  which(x %in% 1:10) # returns only what we want to see
  # But when subsetting, output is equivalent
  x[x %in% 1:10]
  x[which(x %in% 1:10)]

#-------------------------------------
#----- Step 5: custom functions
#-------------------------------------
  
  # You can create your own functions using a general template of
  # your.name <- function(arguments) { expression return(result) }
  
  sum.of.squares <- function(x, y) x^2 + y^2
  sum.of.squares(1)
  sum.of.squares(1, 1)
  
  hello.name <- function(name) print(paste("Hello", name))
  hello.name(world)
  hello.name("world")
  hello.name(cats$coat)
  
  matrix.col.mean <- function(matrix) {
    means <- rep(NA, ncol(matrix)) 
    for (i in 1:ncol(matrix)) {
      means[i] <- mean(matrix[, i])
      }
  }
  y <- cbind(1:6, 5:10)
  matrix.col.mean(y) # No result is returned
  
  # That's because by default any fucntion returns only the last computed value
  # Note the the object "means" did not appear in global environment
  # That's because each function has its own environment
  # and will return to global environment only values of objects inside return() or the ones evaluated last inside function
  
  # To make it return an object created inside function environment we need to use return()
  matrix.col.mean <- function(matrix) {
    means <- rep(NA, ncol(matrix)) 
    for (i in 1:ncol(matrix)) {
      means[i] <- mean(matrix[, i])
    }
    return(means)
  }
  matrix.col.mean(y)
  means.y <- matrix.col.mean(y) # Save function output into an object
  
  
  
  
  
  
  
    