##---------------------------------------------------
##---------------------------------------------------
##----- Part 2: Basic data management in R
##---------------------------------------------------
##---------------------------------------------------

#-------------------------------------------------------------------------
#---- Step 1: prepare the environment, load libraries and import data
#-------------------------------------------------------------------------
  # Wipe the environment clean
  rm(list = ls())
  # Clean the console (in RStudio you can do the same by pressing Ctrl+L)
  cat("\f") 

  # Prepare needed libraries
  library.list <- c("readxl", "stargazer", "microbenchmark")
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
  orders <- read_excel("../data/sample.data.orders.xlsx") #../ means go up one level from current directory
  
  # Alternatively, you could simply tell R where the file is
  orders <- read_excel(file.choose())
  warnings()
  
  
#-------------------------------------------------------------------------
#---- Step 2: look at basic features of our data
#-------------------------------------------------------------------------

  # Structure of our data
  str(orders)
  
  # read_excel creates a tible as its output - an enhanced data.frame
  # But not all functions recognize tibbles yet
  # So we'll make it a regular dataframe
  orders <- as.data.frame(orders)
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
            , out = "summary.txt"
            )
  
  
#-------------------------------------------------------------------------
#---- Step 3: rename variables
#-------------------------------------------------------------------------  
  
  # Our variables have spaces and upper case letters, which maybe confusing. 
  # Let's fix that
  
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
  x <- c("abc", "c,aB", "BA,C")
  gsub("a", "ZZZ", x) # gsub("search", "replace", in)
  gsub("ab","ZZZ", x)
  gsub("ab","ZZZ", x, ignore.case = TRUE)
  gsub("a|b", "ZZZ", x) # replace "a" or "b"
  gsub("[a,b]", "ZZZ", x) # replace any character inside [], including ,
  gsub("[ab]", "ZZZ", x) # replaces any instance of "a" or "b", but not "ab"
  
  # Example 2:
  y <- c(27.5, "$28.44", "$ 18.75")
  mean(y) # mean of y is not defined
  str(y) # becayse y is not a vector of numbers, it's a vector of characters
  gsub("$|$ ", "", y) # Nothing happens
  gsub("$| ", "", y) # Space is removed, but not $
  
  gsub("\\$| ", "", y) # Need to escape system symbol $
  gsub("[$ ]", "", y) # Same thing, but more visually appealing than with \\
  y <- gsub("[$ ]", "", y)
  y
  mean(y)
  y <- as.numeric(y)
  mean(y)

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
  View(orders["sales"]) # returns a mini-dataframe
  View(orders[, "sales"]) # returns a vector of values
  identical(orders["sales"], orders[, "sales"])
  all.equal(orders["sales"], orders[, "sales"])
  mean(orders["sales"]) # doesn't work because mean() can't take a dataframe as an argument
  mean(orders[, "sales"]) # works fine
  
  orders[1:10, "sales"]
  orders[1:10, ] # same as head(orders, n = 10)
  orders[1:10] # All rows for first 10 columns
  
  # How to select a single specific variable by number
  names(orders)[19] # same as colnames() for a dataframe
  View(orders[19])
  View(orders[, 19])
  View(orders[, -19]) # Note that orders[, -"sales"] will not work
  orders[1:10, 6:10]
  
  # Extracting the values inside a column
  orders$shipping.cost
  orders$sales
  View(orders$sales)
  identical(orders$sales, orders[, "sales"])
  str(orders$sales)
  str(orders[, "sales"])
  # Most functions will accept both, but some will only work with a vector of values (i.e. $sales)
  summary(orders$sales)
  summary(orders[, "sales"])
  summary(orders["sales"])
  mean(orders$sales)
  mean(orders[, "sales"])
  mean(orders["sales"])
  quantile(orders$sales, 0.9)
  quantile(orders[, "sales"], 0.9)
  quantile(orders["sales"])
  hist(orders$sales)
  hist(orders[, "sales"])
  hist(orders["sales"])
  
  # How to select a subset of variables/rows manually
  View(orders[c("sales", "quantity")]) # Not recommended, see above
  View(orders[, c("sales", "quantity")])
  identical(orders[c("sales", "quantity")], orders[, c("sales", "quantity")])
  View(orders[, c(2:5,18,19)]) # better not to use it
  View(orders[1:10, c("sales", "quantity")])
  
  # How to select a subset of variables from a certain list
  vars <- c("sales", "quantity")
  View(orders[vars])
  View(orders[, vars])
  # Check which column names are inside vars 
  names(orders) %in% vars # names() and colnames() are the same for data.frame
  View(orders[, names(orders) %in% vars])
  View(orders[, !names(orders) %in% vars])
  
  summary(orders[, vars])
  
  # Create new variables from existing ones
  orders$profit.per.item <- orders$profit / orders$quantity
  orders[, "profit.per.item"] <- orders[, "profit"] / orders[, "quantity"] # same
  summary(orders$profit.per.item)
  
  # Create a variable with insufficient observations
  orders$bad.variable <- c("First", "Second", "Third")
  orders$bad.variable <- 1:10
  orders$bad.variable <- 1:102580
  
  # Create an empty the variable
  orders$empty.variable <- NA
  # This is NOT the same as
  orders$empty.string.variable <- ""
  
  # Drop existing variables
  orders$row.id <- NULL # One at a time
  orders[, c("bad.variable" # Several variables at once
             , "empty.variable"
             , "empty.string.variable"
             )
         ] <- NULL
  
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
  
  # Mixing and matching conditions using &, |, !:
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
  orders$country
  countries <- unique(orders$country)
  countries
  length(countries)
  
  # How can we get an alphabetical ordering of all our unique countries?
  order(countries)
  countries[c(6, 67, 80)]
  countries[c(1, 5, 8)]
  countries[order(countries)]
  countries <- countries[order(countries)]
  countries
  
  # How many cities do we ship to?
  length(unique(orders$city))
  nrow(unique(orders$city)) # doesn't work because nrow() expects a matrix/dataframe
  NROW(unique(orders$city)) # this works
  
  # Do we have any missing values anywhere?
  anyNA(orders) 
  # Which variables have missing values?
  anyNA(orders$order.date)
  anyNA(orders$customer.name)
  anyNA(orders$postal.code)
  
  # Applying the same function to a list of variables
  mean(orders)
  sapply(orders, mean)
  # Same can be achieved by using a less elegant loop:
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
  unique(orders[!is.na(orders$postal.code), "country"])
  
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
  customers <- aggregate(cbind(sales,profit) ~ customer.id, orders, sum)
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
  NROW(unique(orders[, c("city")]))
  nrow(unique(orders[, c("city", "country")]))
  cities <- aggregate(cbind(sales, profit) ~ city + country, orders, sum)
  
#-------------------------------------------------------------------------
#---- Step 6: data merging
#-------------------------------------------------------------------------  
  
  # What if we decided to add customer names and countries to this dataframe with total sales per customer
  # In base R the better way to do it is through merge function

  # First rename the id variable in customers to something els
  colnames(customers)[1] <- "c.id"
  customers <- merge(customers, orders[, c("customer.id", "customer.name", "country")]
                     , by.x = "c.id", by.y = "customer.id" # if same name, use by = "name" instead
                     , all.x = FALSE, all.y = FALSE # INNER join
                     #, all.x = TRUE, all.y = FALSE # LEFT join
                     #, all.x = FALSE, all.y = TRUE # RIGHT join
                     #, all = TRUE # FULL OUTER join
                     ) 
                    
  # The problem is, we now have duplicate values
  View(customers)
  # This happens because for every customer.id in orders we have duplicates of name/country values
  # since the same customer orders multiple times
  # So we need to remove those duplicates somehow
  
  # One way is to remove them from the end result
  customers <- unique(customers)
  # If you expect this to happen, it is more efficient to apply unique() directly to orders[] inside merge() function:
  customers1 <- merge(customers1, unique(orders[, c("customer.id"
                                                    , "customer.name"
                                                    , "country"
                                                    )
                                                ]
                                         )
                     , by = c("customer.id")
                     , all.x = FALSE, all.y = FALSE)
  # Note that this no longer produces exactly identical results
  identical(customers, customers1)
  # That's because the rownames are generated differently in two cases:
  all.equal(customers, customers1)
  # The contents of the dataframes is identical per variable:
  identical(customers[, 3], customers1[, 3])
  
  
  # Now, let's do another example - average profit margings per subcategory
  # First, let's create a profit margin
  orders$profit.margin <- orders$profit/orders$sales
  # We could try aggregating things as they are
  pm.raw <- aggregate(profit.margin ~ category + sub.category
                      , orders
                      , mean
                      )
  # Rename last variable
  colnames(pm.raw)[ncol(pm.raw)] <- "weighted.average"
  
  # However, this aggegation does not take into account the volume of sales
  # A better measure would be weighted average, using sales per subcategory as weight
  
  # First, we need to calculate total sales per subcategory
  sales.subcat <- aggregate(sales ~ category + sub.category, orders, sum)
  # Rename the total value variable to make it unique (it's always the last column)
  colnames(sales.subcat)[ncol(sales.subcat)] <- "sales.total"
  # Next we need to add that number to our original data
  orders <- merge(orders, sales.subcat)
  # Calculate weights as sales per order over sales per subcategory
  orders$weights <- orders$sales/orders$sales.total
  # Create proper weighted averages
  pm.weighted <- aggregate(profit.margin*weights ~ category + sub.category
                           , orders
                           , sum
                           )
  # Rename last variable
  colnames(pm.weighted)[ncol(pm.weighted)] <- "weighted.average"
  
  # Note that we can also avoid adding "weights" as extra variable into the data by using
  pm.weighted.alt <- aggregate(profit.margin*sales/sales.total ~ category + sub.category
                               , orders
                               , sum
                               )
  colnames(pm.weighted.alt)[ncol(pm.weighted.alt)] <- "weighted.average"
  all.equal(pm.weighted, pm.weighted.alt)
  
  # Remove extra variables
  orders[, c("sales.subcat", "weights", "sales.total")] <- NULL
  
  
  # The big issue here is that you have to duplicate/modify code above 
  # whenever you want a diffent weighted average for subgroups
  # So let's fix it by creating a funciton that does is automatically for us
  
  gwa <- function(data, target, weight, group) { #gwa = group weighted average
    # Function arguments:
    #-- data = name of the dataframe
    #-- target = variable that we want the weighted average for (e.g. profit.margin)
    #-- weight = varialbe that will be used to calcualte weights (e.g. sales)
    #-- group = variable(s) that are used to define groups for which to calculate weights (i.e. category/sub.category)
    
    # First, caclulate totals per group of the "weight" variable
    group.totals <- aggregate(as.formula(paste(weight # Creates a formula for aggregation using "weight" variable
                                               , " ~ "
                                               , paste(group, collapse = " + ") # and all "group" variables
                                               , sep = ""
                                               )
                                         ) 
                              , data
                              , sum
                              )
    # Rename last variable to not have the same name as in main data
    weight.total <- paste(weight,".total", sep = "")
    colnames(group.totals)[ncol(group.totals)] <- weight.total
    # Add totals to main data (but only use needed columns to save memory)
    data <- merge(data[, c(target, weight, group)], group.totals)
    # Create a "weights" variable by dividing weight variable by its total per group
    data$weights <- data[, weight]/data[, weight.total]
    # Calculate weighted averages
    weighted.averages <- aggregate(as.formula(paste(target, "*", "weights"
                                                   , " ~ "
                                                   , paste(group, collapse = " + ")
                                                   , sep = ""
                                                   )
                                             )
                                   , data
                                   , sum
                                   )
    # Rename last column
    colnames(weighted.averages)[ncol(weighted.averages)] <- "weighted.average"
    return(weighted.averages)
  }
  
  # Let's try it with the same profit.margin variable as before
  pm.weighted.gwa <- gwa(orders, "profit.margin", "sales", c("category", "sub.category"))
  all.equal(pm.weighted, pm.weighted.gwa)
  
  # We can know easily calculate other weighted averages
  # For example, profit margin weighted using sales across markets and categories
  pm.weighted <- gwa(orders, "profit.margin", "sales", c("market", "category"))
  
#-------------------------------------------------------------------------
#---- Step 7: working with strings
#---  Read more here: http://www.gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf
#-------------------------------------------------------------------------   
  
  # What if we would like to have customer names be in a "Last name, First name" format?
  # We will need to use some string manipulation here
  # Read more in this tutorial: http://www.gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf
  # We'll use strsplit() function
  strsplit("Diffent_ways to.split-words", " ")
  strsplit("Diffent_ways to.split-words", " |_")
  strsplit("Diffent_ways to.split-words", " |_|.")
  strsplit("Diffent_ways to.split-words", " |_|\\.")
  strsplit("Diffent_ways to.split-words", " |_|\\.|-")
  
  # In our case we have to do the following:
  names <- strsplit(customers$customer.name, " ")
  View(names)
  str(names)
  
  # Because strsplit() returns a list of vectors, one needs special indexing to get the values
  View(names[1])
  View(names[[1]])
  View(names[[1]][1])
  View(sapply(names, function(x) x[1]))
  View(sapply(names, function(x) x[2]))
  
  # So now we have to create a new string that will have "Last, First" format
  # To do this we'll use paste() function, which concatenates multiple elements into a single string
  paste("Last", ", ", "First")
  paste("Last", ", ", "First", sep = "")
  paste("Last", ", ", "First", sep = "_")
  names.new <- paste(sapply(names, function(x) x[2])
                     , ", "
                     , sapply(names, function(x) x[1])
                     , sep = ""
                     )
  View(names.new)
  
  # Note that because we used "customer.name" from "customers" dataframe for splitting,
  # the order of new names matched exactly the old names, so we can simply bind them together:
  customers1 <- cbind(customers1, names.new, stringsAsFactors = FALSE)
  
  # But this is prone to errors if somehow the order of "names" list gets changed in the process (e.g. sorted)
  # So it's better to do the whole procedure inside "customers" dataframe:
  customers$names.new <- paste(sapply(strsplit(customers$customer.name, " ")
                                      , function(x) x[2]
                                      )
                               , ", "
                               , sapply(strsplit(customers$customer.name, " ")
                                        , function(x) x[1]
                                        )
                               , sep = ""
                               )
  identical(customers$names.new, customers1$names.new)
  
  
  # What if we want to see the share of phones' brands among all our orders?
  # We have a phone subcatetory, but not phone brand
  # But we can look which product names for phones contain certain brand
  # This can be done using function grepl()
  grepl("pattern", c("pattern", "Pattern", "nothing", "something"))
  grepl("pattern", c("pattern", "Pattern", "nothing", "something"), ignore.case = TRUE)
  
  # Let's look at sales of phones for different brands
  View(orders[grepl("Samsung", orders$product.name, ignore.case = TRUE), ])
  unique(orders[grepl("Samsung", orders$product.name, ignore.case = TRUE), "sub.category"])
  View(orders[grepl("Apple", orders$product.name, ignore.case = TRUE), ])
  unique(orders[grepl("Apple", orders$product.name, ignore.case = TRUE), "sub.category"])
  
  # You can save the results of grepl() if you need to use it more than once to save time
  samsung <- grepl("Samsung", orders$product.name, ignore.case = TRUE)
  apple <- grepl("Apple", orders$product.name, ignore.case = TRUE)
  View(samsung)
  View(apple)
  View(orders[samsung
              , c("order.date"
                  , "category"
                  , "sub.category"
                  , "product.name"
                  , "sales"
                  )
              ]
       )
  mean(orders$sales[samsung])
  mean(orders$sales[apple])
  
  
  