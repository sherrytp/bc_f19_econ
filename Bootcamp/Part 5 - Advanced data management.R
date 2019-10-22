##--------------------------------------------------------------------
##--------------------------------------------------------------------
##----- Data management with data.table
##----- https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
##--------------------------------------------------------------------
##--------------------------------------------------------------------

#-------------------------------------------------------------------------
#---- Part 1: prepare the environment, load libraries and import data
#-------------------------------------------------------------------------
  # Prepare workspace
  rm(list = ls()) # Clear environment
  gc()            # Clear unused memory
  cat("\f")       # Clear the console

  
  # Prepare needed libraries
  library.list <- c("readxl"
                    , "data.table"
                    , "microbenchmark" # To measure execution time
                    )
  for (i in 1:length(library.list)) {
    if (!library.list[i] %in% rownames(installed.packages())) {
      install.packages(library.list[i])
    }
    library(library.list[i], character.only = TRUE)
  }
  rm(library.list)
  
  # Set working directory
  # setwd("")

#-------------------------------------------------------------------------
#---- Part 2: introducing  data.table
#-------------------------------------------------------------------------
  #--------------------------------
  # 2.1
  #--------------------------------
    # The first advantage of data.table is its own fast data import function fread()
    system.time(sales <- read.csv("iowa.liquor.r.hw1.csv"
                                  , check.names = FALSE
                                  , stringsAsFactors = FALSE
                                  , na.strings = ""
                                  )
                , gcFirst = TRUE
                )
    rm(sales)
    gc()
    system.time(sales1 <- fread("iowa.liquor.r.hw1.csv")
                , gcFirst = TRUE
                )
    
    # data.table also has a fast write function fwrite()
    system.time(write.csv(sales1, "sales.csv"), gcFirst = TRUE)
    system.time(fwrite(sales1, "sales1a.csv"), gcFirst = TRUE)
    rm(sales, sales1)
    gc()
    
  #--------------------------------
  # 2.2
  #--------------------------------
    # Load the sample orders data
    orders <- read_excel(file.choose())
    str(orders)
    
    # read_excel() returns a tibble (improved data.frame), so we need to convert it into a data.table
    orders <- as.data.table(orders)
    str(orders)
    
    # Our variables have spaces and upper case letters, which maybe confusing
    # Let's fix that
    old.names <- colnames(orders)
    new.names <- tolower(old.names)
    new.names <- gsub(" |-", ".", new.names)
    
    # data.table uses a modified, more efficient function that can rename variables
    setnames(orders, old.names, new.names)
    
    head(orders)
    # Unlike data.frames, columns of character type are never converted to factors by default
    # (so no need for StringsAsFactors = FALSE)
    # Row numbers are printed with a ":" in order to visually separate the row number from the first column
    # data.table doesn't set or use row names, ever (more details on why later)
    
    # Let's remove some variables that we don't need
    # data.table supports all base syntax of data.frames, e.g. using $ for variable calls
    # So one can do
    orders$row.id <- NULL
    # or when dropping several variables
    drop.names <- c("order.id"
                    , "postal.code"
                    , "product.id"
                    )
    orders[, drop.names] <- NULL
    # But there will be a better way to do it once we get to "By reference" section
  
#-------------------------------------------------------------------------
#---- Part 3: basic syntax
#-------------------------------------------------------------------------
  # General data.table syntax allows much more than simply subsetting rows and colums in a data.frame
  # The syntax works as follows: DT[i, j, by]
  # The way to read it is: Take DT, subset rows using "i", then calculate "j", grouped by "by".
  # DT[ i,  j,  by ] # + extra arguments
  #     |   |   |
  #     |   |    -------> grouped by what?
  #     |    -------> what to do?
  #      ---> on which rows?
    
  
  #--------------------------------
  # 3.1. Subset rows in "i"
  #--------------------------------
    ans <- orders[segment == "Consumer" & region == "Central US"]
    head(ans)
    # No need to use prefix "orders$" everytime we refer to a column (although you can)
    # There is only "i" part in that DT command, so only row indices that satisfy our condition are computed
    # and then simply returned
    # No need for a commma after row indices, although you can use it (in data.frame it is necessary)
    
    # Select only some numbered rows:
    ans <- orders[1:3]
    head(ans)
    # This is different from a data.frame, since data.frame[1:3] will return ALL rows, but only first 3 columns
    ans <- as.data.frame(orders)[1:3]
    head(ans)
  
  #--------------------------------
  # 3.2. Select columns in "j"
  #--------------------------------
    # Selectin a single colum returns a vector, not a data.table;
    # same as orders$customer.name or orders[, "customer.name"]
    ans <- orders[, customer.name]
    head(ans)
    str(ans)
    ans <- as.data.frame(orders)[, "customer.name"]
    head(ans)
    str(ans)
    # We don't need to subset rows, so there's nothing in "i", but we need to have a comma before "j"
    orders[customer.name] # This will not work
    
    # What about selecting multiple columns?
    ans <- orders[, c(customer.name, country)] 
    head(ans)
    str(ans)
    View(ans)
    # Again, the above returns a vector, but now both columns are merged into one vector
    # This is not what anyone wants
    
    # You could still do it the regular data.frame way:
    ans <- orders[, c("customer.name", "country")]
    str(ans)
    head(ans)
    
    # But a proper way to return a subset of columns as a data.table is to use this:
    ans <- orders[, .(customer.name, country)]
    head(ans)
    str(ans)
    # Here .() is a shortcut for list() - it allows much richer set of option to be applied to columns
    # We'll see a lot of use for it below
    
    # You can also select a subset of column by specifying start and end column:
    ans <- orders[, customer.name:country]
    head(ans)      
    str(ans)
    
    # This can also be done in conjunction with !
    # (note that we need parenthesis to make ! apply to the entire sequence of colums):
    ans <- orders[, !(customer.name:country)]
    head(ans)
  

  #--------------------------------
  # 3.3. Compute or do in "j"
  #--------------------------------
    # Since data.table referes to columns in "j" as variables/objects,
    # we can apply all kinds of functions to those variables
    
    # How many orders have a non-zero discount?
    ans <- orders[, sum(discount != 0)]
    ans
    # Here data.table first identifies as 1/0 the rows for which discount is non-zero
    # and then sums up the resulting ones and zeros to get the number of non-zero discounts
    
    # You can use "j" to return something as simple as a count/sum/average of a variable
    ans <- orders[, mean(sales)]
    ans
    # You can do it only for a subset of rows:
    ans <- orders[segment == "Consumer" & country == "United States"
                              , mean(sales)
                  ]
    ans
    # The important nuance here is that data.table calculates the result in 3 steps
    # First it identifies the row indices that satisfy to the condition
    # Second it selects only sales column
    # Third it calculates the fucntion mean() over sales for only matching rows
    # This is more efficient than data.frame approach, notably so for big data
    # A data.frame way would be
    ans <- mean(orders[orders$segment == "Consumer" & orders$country == "United States"
                                   , orders$sales
                                   ]
                            )
    ans
    
    # data.table is faster
    microbenchmark("DT: " = orders[segment == "Consumer" & country == "United States"
                                      , mean(sales)
                                      ]
                 , "DF: " = mean(as.data.frame(orders)[orders$segment == "Consumer" & orders$country == "United States"
                                            , "sales"
                                            ]
                                     )
                 , setup = set.seed(100)
                 )
    
    # You can selec combinations of existing and new variables (impossible with data.frames)
    ans <- orders[, .(profit , profit.margin = profit/sales)]
    head(ans)
    
    # You can  calculate average profits and sales for all discounted orders in USA
    # And have the results stored in named variables
    ans <- orders[discount != 0 & country == "United States"
                  , .(aver.sales = mean(sales)
                      , aver.profit = median(profit)
                      )
                  ]
    ans
    str(ans)
    
    
#-------------------------------------------------------------------------
#---- Part 4: Aggregations
#-------------------------------------------------------------------------
  #--------------------------------
  # 4.1. Grouping using "by"
  #--------------------------------

    # data.table has a special variable .N to count the number of rows in the current group
    # It is particularly useful when combined with "by" as we'll see below
    # In the absence of group by operations, it simply returns the number of rows in the subset.
    View(orders[, .N])
    
    # Going back to our example with number of non-zero discounts
    ans <- orders[discount != 0, .N]
    str(ans)
    ans
    # A data frame way would be 
    length(which(orders$discount != 0))
    # or
    nrow(orders[orders$discount != 0, ])
    # But all those methods result in different execution times
    microbenchmark("DT.1: " = orders[, sum(discount != 0)]
                   , "DT.2: " = orders[discount != 0, .N]
                   , "DF.1: " = length(which(as.data.frame(orders)$discount != 0))
                   , "DF.2: " = nrow(as.data.frame(orders)[orders$discount != 0, ])
                   , setup = set.seed(100)
                   )
                   
    # Let's look at how many orders we had in December 2014
    ans <- orders[month(order.date) == 12 
                  & year(order.date) == 2014
                  , .N]
    ans
    # The benefit of that is that the entire dataset is not being subsetted (no partial copy is stored in memory)
    # .N simply counts the number of indices that satisfy the condition.
    microbenchmark("DT: " = orders[month(order.date) == 12 & year(order.date) == 2014
                                       , .N
                                       ]
                   , "DF: " = nrow(as.data.frame(orders)[month(orders$order.date) == 12 & year(orders$order.date) == 2014
                                              , 
                                              ]
                                       )
                   , setup = set.seed(100)
                   )
    
    # Let's suppose we want to find the number of orders per country
    # There are several ways to do it syntax-wise, the best being
    ans <- orders[, .N, by = .(country)]
    str(ans)
    head(ans)
    # Note that you can skip .() around .N here, because the result of grouping is always a data.table
    
    # When there is only one grouping variable, we can omit .() in "by"
    ans <- orders[, .N, by = segment]
    ans
    
    # You could also you a combination of variables to create multi-level groupings
    ans <- orders[, .N, by = .(country, segment)]
    head(ans)
    
    # One can perform all kinds of calculations in "j" by groups specified in "by"
    ans <- orders[, .(total.orders = .N
                      , average.sales = mean(sales)
                      , average.profit = median(profit)
                      )
                  , by = .(country, segment)
                  ]
    head(ans)
    
    # This is something that is impossible to do via simple data.frame methods
    # You will need to use aggregate 3 times, and then merge/rbind
    
        
  #--------------------------------
  # 4.2. Chaining
  #--------------------------------
    # data.table supports chaining as a way to simplify the code
    # and avoid storing intermediate results in memory
    
    # Suppose we again want to see average sales and profits for orders by countries
    # But this time we want only countries that have at least 500 orders total per segment
    # We could do it in two steps: first get all countries
    ans <- orders[, .(total.orders = .N)
                  , by = .(country, segment)]
    head(ans)
    # And then filter out countries with more than 500 total orders per segment:
    ans <- ans[total.orders > 500]
    head(ans)
    nrow(ans)
    # This, however, makes R store intermediate result in memory, while we actually don't need it
    
    # A better way to do it is to use chaining:
    ans <- orders[, .(total.orders = .N), by = .(country, segment)
                  ][total.orders > 500
                    ]
    head(ans)
    
    # This is very convenient when you need to do several consecuative actions
    # For example, consider the following aggregation:
    ans <- orders[, .(total.orders = .N
                      , average.sales = mean(sales)
                      , average.profit = mean(profit)
                      )
                  , by = .(country, segment)
                  ]
    head(ans)
    # We would like to sort the results by the same variables we use for grouping
    x <- c(1, 5, 7, 2, 11)
    order(x)
    x[order(x)]
    
    # We can do it by chaining in the following way:
    ans <- orders[, .(total.orders = .N
                      , average.sales = mean(sales)
                      , average.profit = mean(profit)
                      )
                  , by = .(country, segment)
                  ][order(total.orders
                          , average.sales
                          , average.profit
                          , decreasing = TRUE)]
    head(ans)
    # Note that inside data.table order is performed using highly optimized internal data.table sorting method
    # We can also use data.table specific command to reorder by reference (i.e. most efficient)
    setorder(x)
    
  #--------------------------------
  # 4.3. Expressions in "by" and multiple columns in "j"
  #--------------------------------
    # Grouping in "by" can be done according to any criterion that assigns each row to a certain group
    # For example, we may want to see how many orders give us positive profit per segment
    ans <- orders[, .N, by = .(segment, profit > 0)]
    head(ans)
    
    # Suppose we want to calculate mean for a lot of columns in grouped data
    # We can do it column by column, like so:
    ans <- orders[, .(total.orders = .N
                      , average.sales = mean(sales)
                      , average.profit = mean(profit)
                      , average.discount = mean(discount)
                      , average.shipping = mean(shipping.cost)
                      )
                  , by = .(country, segment)
                  ]
    head(ans)
    # But it is too inconveniet for more than a couple of columns
    
    # A better way is to use another special symbol of data.table - .SD
    # .SD stands for "Subset of Data" and it itself a data.table with all the data for the current group
    # By default it can be used to simply return all columns except the ones used for grouping
    ans <- orders[, .SD, by = .(country, segment)
                  ][order(country, segment)
                    ]
    head(ans)
    # This essentially only rearranges the entire dataset according to grouping variables
    
    # You can use it to return only a subset of .SD for each group
    ans <- orders[, head(.SD, 2), by = .(country, segment)
                  ][order(country, segment)
                    ]
    head(ans)
    
    # Suppose we only want certain columns to be reported for each group
    # We can then use .SDcols to specify what should be inside .SD for each group
    # .SDcols only accepts either column names or indices, which is different from "i", "j" or "by"
    ans <- orders[, .SD
                  , by = .(country, segment)
                  , .SDcols = c("profit"
                                , "sales"
                                , "discount"
                                , "shipping.cost"
                                ) 
                  ]
    head(ans)
    
    # We can apply the same functions to all columns using lapply()
    # This is because lapply() applies function to elements of a list, and data.table is a list of vectors
    ans <- orders[, lapply(.SD, mean)
                  , by = .(country, segment)
                  , .SDcols = c("profit"
                                , "sales"
                                , "discount"
                                , "shipping.cost")
                  ]
    head(ans)

#-------------------------------------------------------------------------
#---- Part 5: reference symantics using ":=" operator
#-------------------------------------------------------------------------
  #--------------------------------
  # 5.1. Delete columns by reference
  #--------------------------------
	  drop.names <- c("row.id"
	  								, "order.id"
	  								, "postal.code"
	                  , "product.id"
	                  )
	  # We could use the data.frame way to drop those variables
	  orders[, drop.names] <- NULL
	  # But data.table has a much better way to do it via ":="
	  orders[, (drop.names) := NULL]
	  # This way no extra memory is used in any way, unlike base R
  
  #--------------------------------
  # 5.2. Add columns by reference
  #--------------------------------
	 	# The ":=" operator should generally be used inside [] to make any changes to columns in a data.table
	  # There are two ways to use is: (a) LHS := RHS (b) ':='( ... )
	  
	  # Adding columns the usual way of "j", this creates a new data.table as a result
	  orders[, .(profit.margin = profit/sales
	             , discount.margin = discount/sales
	             )
	         ]
	  
	  # But what if we want hese new variables added to our existing dataset? 
	  # we could do it the standard way: 
	  orders[, "profit.margin"] <- orders[, profit]/orders[, sales]
	  orders[, "discount.margin"] <- orders[, discount]/orders[, sales]
	  
	  # But that is not most effieicnet way, as you are effectively openning your data 3 times 
	  orders[, c("profit.margin", "discount.margin") := NULL] 
	  
	  # Adding columns by reference, variant (a), this does not create a new data.table
	  orders[, c("profit.margin", "discount.margin") := .(profit/sales, discount/sales)]
	  # Note that we need .() because we need a list inside "j"
	  
	  # The result of ":=" is returned invisibly, so you can either use View()/head()
	  # Or you can add [] at the end of data table command to have it printed
	  orders[, c("profit.margin"
	             , "discount.margin"
	             ) := .(profit/sales
	                    , discount/sales
	                    )
	         ][]
	  
	  # Add columns by reference, variant (b)
	  orders[, .(profit.margin, discount.margin) := NULL]
	  orders[, c("profit.margin", "discount.margin") := NULL]
	  orders[, ':='(profit.margin = profit/sales        # Can leave comments after each line
	  							, discount.margin = discount/sales  # Can leave comments after each line
	  							)
	  			 ]
	  head(orders[, .(profit.margin, discount.margin)])
	  # Generally, you would like to use variant (b) in most cases

	    
  #--------------------------------
  # 5.3. Update columns by reference
  #--------------------------------
	  # Note that new columns will be printed in scientific notation
	  head(orders[, .(profit.margin, discount.margin)])
	  # That's because by default R uses that notation for long decimals
	  # To disable that, you can use the command
	  options(scipen = 999) 
	  head(orders[, .(profit.margin, discount.margin)])
	  # But it's better to simply round it to first three digits
	  options(scipen = 0)
	  orders[, ':='(profit.margin = round(profit.margin, 3)
	  							, discount.margin = round(discount.margin, 3)
	  							)
	  			 ]
	  head(orders[, .(profit.margin, discount.margin)])
	  
	  # You can use ":=" when subsetting your columns in "i"
	  orders[country == "United States", country := "USA"]
	  # The default R's way (not most efficient to do) would be: 
	  orders[country == "United States", country] <- "USA"
	  
	  # Note that the following is different:
	  orders[country == "United Kingdom"][, country := "UK"]
	  # That's because the line above subsets the data first, and only works with that subset
	  # The result needs to be assigned if you want to use it later
	  ans <- orders[country == "United Kingdom"][, country := "UK"]
	  head(ans[, city:country])
	  # The original table remains unchanged
	  head(orders[country == "United Kingdom", city:country])
	  
	  
  #--------------------------------
  # 5.4. ":=" along with grouping using "by"
  #--------------------------------
	  # Create total and average sales per country
	  # faster and use merge() and aggregate() at the same time 
	  orders[, ':='(total.sales.per.country = sum(sales)
	  							, ave.sales.per.country = mean(sales)
	  							)
	  			 , by = .(country)
	  			 ]
	  
	  # Note that the result is very different when doing that withouth ":="
	  orders[, .(total.sales.per.country = sum(sales)
	  					 , ave.sales.per.country = mean(sales)
	  					 )
	  			 , by = .(country)
	  			 ]
	  
	  # If you need to apply the same function to a bunch of columns, you can do:
	  orders[, c("ave.profit.per.segment"
	  					 , "ave.sales.per.segment"
	  					 , "ave.discount.per.segment"
	  					 , "ave.shipping.cost.per.segment"
	  					 ) := lapply(.SD, mean)
	  			 , by = .(segment)
	  			 , .SDcols = c("profit"
	  			 							, "sales"
	  			 							, "discount"
	  			 							, "shipping.cost"
	  			 							)
	  			 ]
	  # Note that this adds a lot of redundant info to main data
	  # You should avoid doing that for large datasets
	  
#-------------------------------------------------------------------------
#---- Part 6: ":=" and copy()
#-------------------------------------------------------------------------
	  # Remember how we stored old names in a vector at the start of this script?
	  # That vector is supposed to have names like "Order Date" and so on
	  old.names
	  # But it no longer has those
	  # In fact, it has the new names, including the new colums we addded
	  
	  # That's because in base R and in data.table assignemtn "<-" creates a label, not a copy
	  # e.g. old.names is a label for something that contains the names of "orders"
	  # It gets updated whenever "orders" is being updated by reference
	  
	  # The same happens with data.tables:
	  orders1 <- orders
		identical(orders, orders1)  
		orders[, ((ncol(orders) - 8):ncol(orders)) := NULL]
		identical(orders, orders1)
		orders[, ':='(profit.margin = round(profit/sales, 2)
	  							, discount.margin = round(discount/sales, 2)
	  							)
	  			 ]
		identical(orders, orders1)
		
		# But it no longer is the case if you use data.frame methods and not ":="
		orders[, c("profit.margin", "discount.margin")] <- NULL
		identical(orders, orders1)
		all.equal(orders, orders1)
		
		# If you want to actually save a copy of an object, you need to use copy()
		orders1 <- copy(orders)
		orders[, ':='(profit.margin = round(profit/sales, 2)
	  							, discount.margin = round(discount/sales, 2)
	  							)
	  			 ]
		identical(orders, orders1)
		all.equal(orders, orders1)
		rm(orders1)
		
#-------------------------------------------------------------------------
#---- Part 7: Merging in data.table
#-------------------------------------------------------------------------	
	#--------------------------------
  # 7.1. Create a new data that needs to be merged with
  #--------------------------------
		# Data.table offers slightly modified function uniqueN() that counts unique records
		N.customers <- uniqueN(orders[, customer.id]) # Total of 17415 unique customers 
		# the number of those unique records, a special fucntion by data.table 
		# It's advantage is that it will return the number both for vectors (single colums) and data.tables (multiple columns)
		# With dataframe you will need to use length() and nrow() depeding on number of colums
		# For just one column it's slower:
		microbenchmark("DT: " = uniqueN(orders[, customer.id])
		               , "DF.1: " = length(unique(as.data.frame(orders)$customer.id))
		               , "DF.2: " = nrow(unique(as.data.frame(orders)[, "customer.id"]))
		               , setup = set.seed(100)
		               )
		# But for several columns it's much faster:
		microbenchmark("DT: " = uniqueN(orders[, customer.name:country])
		               , "DF: " = nrow(unique(as.data.frame(orders)[, which(colnames(orders) == "customer.name")
		                                                            : which(colnames(orders) == "country")
		                                                            ]
		                                      )
		                               )
		               , setup = set.seed(100)
		               )

		# Let's create a new table with total and average sales/profits per customer
		customers <- orders[, .(total.sales = sum(sales)
														, total.profit = sum(profit)
														, ave.sales = mean(sales)
														, ave.profit = mean(profit)
														)
												, by = .(customer.id)
												]
		head(customers)
		
		# Let's generate new data for customers - age and marital status
		customers.new <- data.table(customer.id = customers[, customer.id]
																, age = round(runif(N.customers, 18, 60), 0)
																, marital.status = round(runif(N.customers, 0, 2), 0)
																)
		str(customers.new)
		summary(customers.new)
		# Replace numerical indicators for marital status with factor values
		customers.new[, marital.status := factor(marital.status
																						 , levels = c(0, 1, 2)
																						 , labels = c("Single", "Married", "Divorced")
																						 )
									]
		str(customers.new)
		summary(customers.new)
		# Now let's make new data not have the same exact customer IDs
		customers.new[grepl("A", customer.id, ignore.case = FALSE)
									, customer.id := gsub("A", "AA", customer.id)
									][grepl("B", customer.id, ignore.case = FALSE)
										, customer.id := gsub("B", "BB", customer.id)
										][grepl("C", customer.id, ignore.case = FALSE)
											, customer.id := gsub("C", "CC", customer.id)
											]
		# And finally let's randomly select only 5000 observations
		set.seed(100)
		customers.new <- customers.new[sample(.N, 5000)]
		
	#--------------------------------
  # 7.2. Look at different types of merging we can do
	#	https://rstudio-pubs-static.s3.amazonaws.com/52230_5ae0d25125b544caab32f75f0360e775.html
  #--------------------------------
		# We now need to merge new customer data with existing customer data
		# Alternatively, we need to add new variables to existing dataset per customer 
		# We are not sure if new data has info on the same customers (it does not)
		# So we need to figure out what kind of final result we want
		
		# We can keep only rows that have matching customers in both old and new data
		merge.dt <- customers[customers.new          # It could also be another way around customers.new[customers, ...]
		                      , on = .(customer.id)   # The ID variable used to identify matches
		                      , nomatch = FALSE       # Only rows that have matching data in both tables are returned
		                      ]
		nrow(merge.dt)
		# This is equivalent to what is known as INNER join in SQL and can also be done using merge() command: 
		merge.df <- merge(customers, customers.new
		                  , by = "customer.id"
		                  , all = FALSE
		                  , sort = FALSE
		                  )
		nrow(merge.df)  
		identical(merge.dt, merge.df) # Because order of observations is different
		all.equal(merge.dt, merge.df)
		identical(merge.dt[order(customer.id)], merge.df[order(customer.id)])
		
		# Note that merge() command above is actually an improved version from data.table
		# I.e. while it looks like a base R function, it's in fact a replacement from data.table
		# So it will work very fast on data.tables and faster on data.frames than R's basic merge
		
		# This time DT looses in time (but it will have other merits, as shown further down below)
		microbenchmark("DT: " = customers[customers.new, on = .(customer.id)
		                                  , nomatch = FALSE
		                                  ]
		               , "DF: " =  merge(as.data.frame(customers)
		                                 , as.data.frame(customers.new)
		                                 , by = "customer.id"
		                                 , all = FALSE
		                                 , sort = FALSE
		                                 )
		               , setup = set.seed(100)
		               )
		
		# Keep only rows that have data in old dataset
		merge.dt <- customers.new[customers
													 , on = .(customer.id)
													 ]
		nrow(merge.dt)
		# This is known as LEFT join in SQL and can also be done using merge() command:
		merge.df <- merge(customers, customers.new
		                  , by = "customer.id"
		                  , all.x = TRUE
		                  , sort = FALSE
		                  )
		nrow(merge.df)
		identical(merge.dt, merge.df) # FALSE because column order is different 
		all.equal(merge.dt, merge.df)
		identical(merge.dt, setcolorder(merge.df, names(merge.dt)))

		# Keep only rows that have data in new dataset
		merge.dt <- customers[customers.new
											 , on = .(customer.id)
											 ]
		nrow(merge.dt)
		# This is known as LEFT join in SQL and can also be done using merge() command:
		merge.df <- merge(customers, customers.new
		                  , by = "customer.id"
		                  , all.y = TRUE
		                  , sort = FALSE
		                  )
		nrow(merge.df)
		identical(merge.dt, merge.df) # FALSE because row order is different 
		all.equal(merge.dt, merge.df)
		identical(merge.dt[order(customer.id)], merge.df[order(customer.id)])
		
		
		# Finally, if you want to keep all data from both sources, you need FULL OUTER join
		# You should do it  with merge() function (data.table way X[Y] needs  complicated adjustments)
		merge.df <- merge(customers, customers.new
		                  , by = "customer.id"
		                  , all = TRUE
		                  )
		nrow(merge.df)
		
		
			
	#--------------------------------
  # 7.3. X[Y] or merge(X,Y)?
  #--------------------------------
		# For recent versions of data.table both work very fast
		# The main difference is that X[Y] allows for "j" argument
		# Which means you can do joins on a subset of columns only and/or caclulate functions of joined data
		# While merge() will first need to join full datasets, and then do anything else
		
		# Suppose you want to calculate profit margin for customers with full info
		# The DF way would first require merging of full dataset
		merge.df <- merge(customers[, -c("ave.sales", "ave.profit")]
		                  , customers.new
		                  , by = "customer.id"
		                  , all = FALSE
		                  , sort = FALSE
		                  )
		# Then calculating profit margins:
		merge.df$profit.margin <- merge.df$total.profit / merge.df$total.sales
		# And then removing unnecessary columns:
		merge.df$total.profit <- NULL
		merge.df$total.sales <- NULL
		# The DT way is simpler, faster and more memory efficient:
		merge.dt <- customers[customers.new
		                      , on = .(customer.id)
		                      , nomatch = FALSE
		                      , .(customer.id
		                          , age
		                          , marital.status
		                          , profit.margin = total.profit/total.sales
		                          )
		                      ]
		identical(merge.dt[order(customer.id)], merge.df[order(customer.id)])
		
		# Suppose we want to calculate total sales and profits for married vs single vs divorced customers
		# The data.frame way would be first merge, then aggregate:
		merge.df <- merge(customers[, -c("ave.sales", "ave.profit")]
		                  , customers.new
		                  , by = "customer.id"
		                  , all = FALSE
		                  , sort = FALSE
		                  )
		agg.df <- aggregate(cbind(total.sales, total.profit) ~ marital.status
		                    , merge.df
		                    , sum
		                    )
		# But this way we have to store full merge in the memory
		# While we actually only need a few aggregated results
		
		# The DT way would be
		agg.dt <- customers[customers.new
		                    , on = .(customer.id)
		                    , nomatch = FALSE
		                    , .(total.sales, total.profit, marital.status)
		                    ][, .(total.sales = sum(total.sales)
		                          , total.profit = sum(total.profit)
		                          )
		                      , by = marital.status
		                      ]
		
		# DT is both faster and more memory efficient:
		microbenchmark("DT: " = customers[customers.new
		                                  , on = .(customer.id)
		                                  , nomatch = FALSE
		                                  , .(total.sales, total.profit, marital.status)
		                                  ][, .(total.sales = sum(total.sales)
		                                        , total.profit = sum(total.profit)
		                                        )
		                                    , by = marital.status
		                                    ]
		               , "DF: " =  aggregate(cbind(total.sales, total.profit) ~ marital.status
		                                     , merge(as.data.frame(customers[, -c("ave.sales", "ave.profit")])
		                                             , as.data.frame(customers.new)
		                                             , by = "customer.id"
		                                             , all = FALSE
		                                             , sort = FALSE
		                                             )
		                                     , sum
		                                     )
		               , setup = set.seed(100)
		               )
		
		
  
  
    