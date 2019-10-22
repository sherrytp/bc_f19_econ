##---------------------------------------------------
##---------------------------------------------------
##----- ADEC 7910: Software Tools for Data Analysis
##----- R Homework 1 Template -----------------------
##------Name: Peng Tian -----------------------------
##------Date: March 17, 2019 ------------------------

##--------------------------------------------------------------------
##----- General
##--------------------------------------------------------------------
  
  # Clear the workspace
  rm(list = ls()) # Clear environment
  gc()            # Clear unused memory
  cat("\f")       # Clear the console
  
  # Prepare needed libraries
  library.list <- c("stringr")
  for (i in 1:length(library.list)) {
    if (!library.list[i] %in% rownames(installed.packages())) {
      install.packages(library.list[i]
                       , repos = "http://cran.rstudio.com/"
                       , dependencies = TRUE
                       )
    }
    library(library.list[i], character.only = TRUE)
  }
  rm(library.list, i)
  
  # Set working directory and path to data
  getwd()
  setwd()
  
  # Load data
  sales <- read.csv(file.choose()
  									, check.names = FALSE
  									, stringsAsFactors = FALSE
  									, na.strings = ""
  									)
##--------------------------------------------------------------------
##----- Q1 - rename variables ----------------------------------------
##--------------------------------------------------------------------
  old.names <- colnames(sales)
  old.names
  new.names <- c("data", "store.number", "store.name", "address", "city", "zip.code", "store.location", "county", 
               "category.name", "item.description", "bottle.volumn.ml", "state.bottle.retail", "bottles.sold", 
               "sale.dollars", "volumn.sold.liters")
  colnames(sales) <- new.names
  View(sales)

    # remove values created for naming 
  rm(new.names, old.names)

##--------------------------------------------------------------------
##----- Q2 - store registry ------------------------------------------
##--------------------------------------------------------------------

  # Q2.1 - Filter out unique store records and drop store info from main data
  storeslist = c("store.number", "store.name", "address", "store.location", "city", "zip.code", "county")
  stores <- sales[,storeslist]
  stores <- unique(stores)
  View(stores)

  NROW(unique(sales$store.number))    # return 1925 
  
    # drop those 7 variables from sales: 
  sales[, storeslist] <- NULL 
  rm(storeslist) 
  
  # Q2.2 - Filter out store GPS coordinates from location variable
    # split the variables in location 
  nms <- strsplit(stores$store.location,"\\(|\\)")
  nms <- sapply(nms, function(x) x[2])

    # split the latitude and longitude in location3 and store it temporarily
  temp <- strsplit(nms, ",")
  stores$store.latitude <- as.numeric(sapply(temp, function(x) x[1]))
  stores$store.longitude <- as.numeric(sapply(temp, function(x) x[2]))
  View(stores)
  
    # delete temporary variables 
  rm(nms, temp)
  
  # Q2.3 - Drop location variable from stores 
  stores[, "store.location"] <- NULL

  # Q2.4 - Average GPS coordinates for stores
  store1 <- aggregate(cbind(store.latitude, store.longitude) ~ store.number, stores, mean)
  stores <- merge(stores[, -c(7,8)], store1
                  , by = "store.number", all.x = TRUE, all.y = FALSE)
  rm(store1)
  # Q2.5 - Removing duplicates
  stores <- unique(stores) 

  # Q2.6 - Fix address, city and county names
  install.packages(stringr) 
  library(stringr)
  
  stores$address <- gsub("[.,,]", "", stores$address)     # move all dots and commas from address 
  stores[, c("address", "city", "county")] <- sapply(stores[, 
            c("address", "city", "county")], str_to_title)
  
  # Q2.7 - Remove duplicates
  stores <- unique(stores)  
  
  # Q2.8 - Fill in missing country names 
  store2 <- stores[stores$county != "", ] 
  stores <- merge(stores, store2[, c(1,6)] 
                  , by = "store.number"
                  , all.x = TRUE, all.y = FALSE)
  stores<-stores[,-6]
  rm(store2)
#  colnames(stores)[8] <- "county"
  
  # Q2.9 - Remove duplicates
  stores <- unique(stores)    
  
  # Q2.10 - Extract duplicates
  stores$dup <- as.integer(duplicated(stores$store.number))
  stores$dup[which(stores$dup != 1)] <- 0

  sum(stores$dup) 
  
  # Q2.11 - Check for proper zip/city/county combinations
	geo <- read.csv(file.choose()
	                , check.names = FALSE
	                , stringsAsFactors = FALSE
	                , na.strings = ""
	                )

  colnames(geo)[1] <- "zip.code"
  geo$match <- 1
  stores <- merge(stores, geo, 
                  by.x = c("city","zip.code","county"), 
                  by.y = c("city","zip.code","county"), 
                all.x = TRUE, all.y = FALSE) 
  stores$match[which(is.na(stores$match))] <- 0 
  stores[, c("match.x", "match.y")] <- NULL
  
##--------------------------------------------------------------------
##----- Q3 - cleaning sales data
##--------------------------------------------------------------------
  
  # Q3.1 - Convert sales and prices to proper format
  sales$state.bottle.retail <- as.numeric(gsub("\\$", "", sales$state.bottle.retail))
  sales$sale.dollars <- as.numeric(gsub("\\$", "", sales$sale.dollars))
  
  # Q3.2 - Create subcategory variable
  sales$subcategory <- sales$category.name
  sales$category.name <- NULL
  
  # Q3.3 - Create new category variable
  last <- word(sales$subcategory, -1)   # Get the last word of each subcategory
  unique(last)
  rm(last)
  
  sales$category[grepl('tequila', sales$subcategory, ignore.case = TRUE)] <- "Tequila" 
  sales$category[grepl('brandies', sales$subcategory, ignore.case = TRUE)] <- "Brandy"
  sales$category[grepl('gin|gins', sales$subcategory, ignore.case = TRUE)] <- "Gin"

##--------------------------------------------------------------------
##----- Q4 - export cleand data
##--------------------------------------------------------------------
  write.csv(sales, "sales.csv")
  write.csv(stores,"stores.csv")
