##--------------------------------------------------------------------
##--------------------------------------------------------------------
##----- MSAE Bootcamp on R, Sep 8, 2018
##----- Part 2: Basic data management
##--------------------------------------------------------------------
##--------------------------------------------------------------------

#-------------------------------------------------------------------------
#---- Step 1: prepare the environment, load libraries and import data
#-------------------------------------------------------------------------
  # Wipe the environment clean
  rm(list = ls())
  # Clean the console (in RStudio you can do the same by pressing Ctrl+L)
  cat("\f") 

  # Prepare needed libraries
  library.list <- c("readxl", "stargazer")
  for (i in 1:length(library.list)) {
    if (!library.list[i] %in% rownames(installed.packages())) {
      install.packages(library.list[i])
    }
    library(library.list[i], character.only = TRUE)
  }
  rm(library.list)
  
  # Set working directory
  setwd("")
  # Load the sample orders data that we used for Excel and Tableau based on location of working directory
  orders <- read_excel("../data/orders.xlsx")
  
  # Alternatively, you could simply tell R where the file is
  orders <- read_excel(file.choose())
  
  # Warnings are non-critical issues, but you still need to look at them if they show up
  warnings()
  
  
  # read_excel creates a tiblle as the result, but we rather want a dataframe
  # So we make it to be one:
  orders <- as.data.frame(orders)
  
#-------------------------------------------------------------------------
#---- Step 2: look at basic features of our data
#-------------------------------------------------------------------------

  # Structure of our data
  str(orders)
  
  # What are the names of the variables in our data?
  colnames(orders)
  
  # Look at 5 first/last observations records of the data
  View(head(orders, n = 5))
  tail(orders, n = 5)

  # What are the dimensions of our data?
  dim(orders)
  nrow(orders)
  ncol(orders)
  
  # Some basic numerical stats
  summary(orders)
  
  # A better way to output summary statistics
  stargazer(orders
            , type = "text" # Type of output - text, HTML or LaTeX
            , summary = TRUE   # We want summary only
            , title = "Descriptive statistics" #Title of my output
            , digits = 2
            , out = "summary.txt")
  
  
#-------------------------------------------------------------------------
#---- Step 3: rename variables
#-------------------------------------------------------------------------  
  
  # Our variables have spaces and upper case letters, which maybe confusing. Let's fix that
  
  # We'll use tolower() function first
  tolower("A chARACter String WITH ALL KiNDs of caSes")
  toupper("A chARACter String WITH ALL KiNDs of caSes")
  
  # We can store old/new names in separate objects (if we need it somewhere else)
  old.names <- colnames(orders)
  old.names
  new.names <- tolower(old.names)
  new.names
  # And then assign them to our data
  colnames(orders) <- new.names
  
  # Or  apply it directly using colnames() function:
  colnames(orders) <- old.names # revert the renaming we did above
  colnames(orders) <- tolower(colnames(orders))
  
  # Now let's replace all spaces and dashes with dots using gsub() function
  # For more info on regular expressions in R see Chapter 5 in this tutorial:
  # http://www.gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf
  # Or a cheat sheet for regular expressions here:
  # https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
  
  # Example 1:
  x <- c("abc", "caB", "BAC")
  gsub("a", "ZZZ", x) # gsub("search", "replace", in)
  gsub("ab","ZZZ", x)
  gsub("ab","ZZZ", x, ignore.case = TRUE)
  gsub("a|b", "ZZZ", x) # replace "a" or "b"
  gsub("[ab]", "ZZZ", x) # replace any character inside []
  
  # Example 2:
  y <- c(27.5, "$28.44", "$ 18.75")
  mean(y) # mean of y is not defined
  str(y) # because y is not a vector of numbers, it's a vector of characters
  gsub("$| ", "", y) # Nothing happens
  gsub("\\$| ", "", y) # Need to escape system sybmol $
  
  # You can use [] to specify all characters that need to be replaced, including special ones
  gsub("[$ ]", "", y) # Same thing, but more visually appealing than with \\
  y <- gsub("[$,$ ]", "", y)
  mean(y)
  y <- as.numeric(y)
  mean(y)
  
  # Note that [] cannot be use to replace whole words,
  z <- c("kitty", "puppy", "puppykitty")
  gsub("kitty|puppy", "awww", z) # works ok
  gsub("[kittypuppy]", "awww", z) # every individual character in "kitty" and "puppy" is replaced

  # Go back to our variable names
  new.names
  new.names <- gsub(" |-", ".", new.names)
  new.names
  colnames(orders) <- new.names
  
  # Again, you could do it all at once
  colnames(orders) <- old.names #revert back to originals
  colnames(orders) <- tolower(gsub(" |-", "."
                                   , colnames(orders)
                                   )
                              )
  # Clear the no-longer-needed things
  rm(old.names)
  rm(new.names)
  rm(x)
  rm(y)
  
#-------------------------------------------------------------------------
#---- Step 4: data subsetting
#-------------------------------------------------------------------------
  
  # How to select a single specific column by name
  View(orders["sales"])
  View(orders[, "sales"])
  identical(orders["sales"], orders[, "sales"])
  orders[1:10, "sales"]
  orders[1:10, ] # same as head(orders, n = 10)
  
  # How to select a single specific variable by number
  names(orders)[19]
  View(orders[19])
  View(orders[, 19])
  View(orders[, -19]) # Note that orders[, -"sales"] will not work
  orders[1:10, 6:10]
  
  # Extracting the values inside a column
  View(orders$sales)
  identical(orders$sales, orders[, "sales"])
  str(orders$sales)
  str(orders[, "sales"])
  # Some functions will accept both, but some will only work with a vector of values (i.e. $sales)
  summary(orders$sales)
  summary(orders[, "sales"])
  mean(orders$sales)
  mean(orders[, "sales"])
  quantile(orders$sales, 0.9)
  quantile(orders[, "sales"], 0.9)
  hist(orders$sales)
  hist(orders[, "sales"])
  
  # How to select a subset of variables/rows manually
  View(orders[c("sales", "quantity")])
  View(orders[, c("sales", "quantity")])
  View(orders[, c(2:5,18,19)]) # better not to use it
  View(orders[1:10, c("sales", "quantity")])
  
  # How to select a subset of variables from a certain list
  vars <- c("sales", "quantity")
  View(orders[vars])
  View(orders[, vars])
  # Check which column names are inside vars 
  names(orders) %in% vars # names() and colnames() are the same for data.frame
  
  View(orders[, names(orders) %in% vars])
  !FALSE
  ! 2*2 == 4
  View(orders[, !names(orders) %in% vars])
  
  summary(orders[, vars])
  
  # Create new variables from existing ones
  orders$profit.per.item <- orders$profit / orders$quantity
  orders[, "profit.per.item"] <- orders$profit / orders$quantity
  summary(orders$profit.per.item)
  
  # Drop existing variables
  orders$row.id <- NULL
  
  # Select observations based on condition
  View(orders$profit > 0)
  # all data for good (aka profitable) orders
  View(orders[orders$profit > 0, ])
  # sales and profits for good orders
  View(orders[orders$profit > 0, c("sales", "profit")])
  # sales and profits for bad orders:
  View(orders[!orders$profit > 0, c("sales", "profit")])
  View(orders[orders$profit > 0 & orders$quantity > 5
  						, c("sales", "profit", "quantity")
  						]
  		 )
  # R logical operators ! (NOT), == (EQUAL), & (AND)
  ! 2 == 4 
  ! 2 == 4 & 3 == 5
  ! (2 == 4 & 3 == 5)  # Here NOT is applied to intersection of two EQUALs
  
  # In case of our data:
  View(orders[!orders$profit > 0
              , c("sales", "profit", "quantity")
              ]
       )
  View(orders[! orders$profit > 0 & orders$quantity > 5
              , c("sales", "profit", "quantity")
              ]
       )
  View(orders[!(orders$profit > 0 & orders$quantity > 5)
              , c("sales", "profit", "quantity")
              ]
       )
  View(orders[orders$profit > 0 | orders$discount > 0
              , c("sales", "profit", "discount")
              ]
       )
  
  # What countries do we ship to?
  summary(orders$country)
  unique(orders$country)
  length(unique(orders$country))
  
  # How many cities do we ship to?
  length(unique(orders$city))
  nrow(unique(orders$city)) # doesn't work because nrow() expects a matrix/dataframe
  NROW(unique(orders$city)) # this works
  
  # What about our entire data?
  dim(unique(orders))
  
  # Do we have any missing values anywhere?
  anyNA(orders) 
  # Which variables have missing values?
  anyNA(orders$order.date)
  anyNA(orders$customer.name)
  anyNA(orders$postal.code)
  
  # Applying the same function to a list of variables
  mean(orders)
  sapply(orders, mean)
  # Same can be achieved by a much less elegant loop:
  for (i in 1:ncol(orders)) {
  	print(paste("Mean of"
  							, colnames(orders)[i]
  							, "="
  							, mean(orders[, i]) 
  							)
  				)
  }
  
  # How to check for missing values, i.e. NAs
  sapply(orders, anyNA)
  View(sapply(orders, anyNA))
  colnames(orders)[sapply(orders, anyNA)]
  
  # Which observations have missing values
  View(is.na(orders$postal.code))
  View(orders[!is.na(orders$postal.code), "country"])
  
  # We can create a subset of data that does not have any missign values
  # But that is not recommended, as it usually makes you loose a lot of data
  orders.no.NAs <- na.omit(orders)
  
#-------------------------------------------------------------------------
#---- Step 5: data aggregation
#-------------------------------------------------------------------------  
  
  # Let's create a separate dataset that summarizes totals sales and profits per customer
  # Out data identifies unique customers by the customer.id variable
  # We need to calculate total sales and total profits for each unique customer.id
  
  # We will use base R's function aggregate
  customers <- aggregate(cbind(sales,profit) ~ customer.id
                         , orders
                         , sum
                         )
  str(customers)
  View(customers)
  
  # Alternatively you could do the same via
  customers1 <- aggregate(orders[, c("sales", "profit")]
                          , by = list(customer.id = orders$customer.id)
  												, sum
                          )
  View(customers1)
  # The results are identical
  identical(customers, customers1)
  
  # Total sales per each unique city
  nrow(unique(orders[, c("city")]))
  nrow(unique(orders[, c("city", "country")]))
  cities <- aggregate(cbind(sales, profit) ~ city + country
                      , orders
                      , sum
                      )
  View(cities)
  
#-------------------------------------------------------------------------
#---- Step 6: data merging
#-------------------------------------------------------------------------  
  
  # What if we decided to add customer names and countries to this dataframe with total sales per customer
  # In base R the better way to do it is through merge function
  customers <- merge(customers
                     , orders[, c("customer.id"
                                  , "customer.name"
                                  , "country"
                                  )
                              ]
                     , by = c("customer.id")
                     , all.x = FALSE, all.y = FALSE)
  
  # The problem is, we now have duplicate values
  nrow(customers)
  View(customers)
  
  # So we need to remove them
  customers <- unique(customers)
  nrow(customers)
  
  # If you expect this to happen, it is more efficient to apply unique() directly to orders[] inside merge() function:
  customers1 <- merge(customers1
                      , unique(orders[, c("customer.id"
                                          , "customer.name"
                                          , "country"
                                          )
                                      ]
                               )
                     , by.x = c("customer.id")
                     , by.y = c("customer.id")
                     , all.x = FALSE, all.y = FALSE
                     )
  nrow(customers1)
  
  # Note that this no longer produces exactly identical results
  identical(customers, customers1)
  
  # That's because the rownames are generated differently in two cases:
  all.equal(customers, customers1)
  
  # The contents of the dataframes is identical per variable:
  identical(customers[, 3], customers1[, 3])

