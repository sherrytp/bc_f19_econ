##--------------------------------------------------------------------
##--------------------------------------------------------------------
##----- MSAE Bootcamp on R, Sep 8, 2018
##----- Part 1: Basics of R and RStudio
##--------------------------------------------------------------------
##--------------------------------------------------------------------



# Using R as calculator
  1+1
	(3 + (5 * (2 ^ 2))) # hard to read
  3 + 5 * 2 ^ 2       # clear, if you remember the rules
  3 + 5 * (2 ^ 2)     # if you forget some rules, this might help
  
exp()
help("exp") # Equivalent to pressing F1
??stringsplit
??exponent 

# Comparisons
!(1+1 == 3)

3 > 2
3 < 2
3 <= 3
3 =< 3
3 == 3.0000000001
3 == 3.000000000000000000001

# Assignment
x <- 1.25
y = x # Should not use "=" in assignment
x <- 2.75

x <- 1
x <- x + 1

x <- exp(x)
# Cannot assign values of non-existing objects 
x <- z + 1 

really.long.variable.name.super.big.long <- 5
.variable.name <- 6
5variable.name <- 10
'variable with a space' <- 1 
# Rules of Naming Variables in R
# no space 
# undiscrose 
# capital letter - camerel rules
# BUT R supports very long variable names and .dot or -dash

# Vectorized operations
x <- 1:5
exp(x)
y <- cbind(1:5, 1:5, 1:5)
y <- cbind(3:5, 6:8, 9:11)
exp(y)
mean(y)
mean(y[1,1])

# Clear the environment
ls()  # list all objects in the current environment 
rm(y)  # remove values and objects
list <- ls()  # assign all objects and values to "list"
rm(list = ls()) # Clears the entire environment and you NEED to run this line everytime before scatch

x <- c(1, 2, 3)
str(x)

cats <- data.frame(coat = c("grey", "black", "cinnamon")
                   , weight = c(2.1, 5.0,3.2)
                   , likes_string = c(1, 0, 1)
                   )

write.csv(x = cats, file = "feline-data.csv", row.names = FALSE)
rm(cats)

cats <- read.csv("feline-data.csv")
str(cats)

cats$coat
View(cats$coat)

cats$weight + 2

cats$weight <- cats$weight + 2

paste("My cat is", cats$coat)
paste("My cat is", cats$coat, sep = "a")
paste("My cat is", cats$coat, sep = " a ")
paste("My cat is a", cats$coat, "one")

cats$weight + cats$coat

typeof(cats$weight)
typeof(cats$likes_string)
typeof(1)
typeof(1L)
typeof(cats$coat)


cats <- data.frame(coat = c("grey", "black", "cinnamon", "cinnamon")
                   , weight = c(2.1, 5.0, 3.2, "2.3 or 2.4")
                   , likes_string = c(1, 0, 1, 1)
                   )
write.csv(x = cats, file = "feline-data.csv", row.names = FALSE)
rm(cats)
cats <- read.csv("feline-data.csv")

str(cats)
typeof(cats$weight)

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

# Factors
typeof(cats$coat)
class(cats$coat)


# Lists
list_example <- list(1, "a", TRUE, 1+4i)
list_example

another_list <- list("Numbers", 1:100, TRUE)
another_list

z <- list(1:5, "a", c(TRUE, FALSE), 1+4i)
z
str(z)
class(z)
typeof(z)
z[1]
str(z[1])
z([3])
z[1:2]
z[(1)]
str(z[[1]])
z[3][2]     # Null because it's invalid 
z[[3]][2]

# Data Frame, which is one and one of only framework that R mainly works with, but internally is of lists
cats <- data.frame(coat = c("grey", "black", "cinnamon")
                   , weight = c(2.1, 5.0, 3.2)
                   , likes_string = c(1, 0, 1)
                   )
cats 
view(cats)
str(cats)
class(cats)
typeof(cats)

# Differences between dataframe AND list: 
# dataframe must have a record of every one in it even though there is (empty)NaN in some cells 

cats[1]
str(cats[1])
cats[1, ]
cats[ ,1]
str(cats[ ,1])
cats[1,2]
cats[2,1]

# work with numerical variables
cats$coat
view(cats$coat)
str(cats$coat)

cats$weight + 2

cats$weight <- cats$weight + 2
mean(cats$weight)

typeof(cats$weight)
typeof(cats$likes_string)

# work with strings/characters 
typeof(cats$coat)
cats$weight + cats$coat 
