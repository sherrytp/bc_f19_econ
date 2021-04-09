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
  rm(library.list)
  
  # Set working directory and path to data, if need be
  # setwd("")
  
  # Load data
  sales <- read.csv(file.choose()
  									, check.names = FALSE
  									, stringsAsFactors = FALSE
  									, na.strings = ""
  									)

##--------------------------------------------------------------------
##----- Q1 - rename variables
##--------------------------------------------------------------------
  
  old.names <- colnames(sales)
  new.names <- tolower(old.names)
  new.names <- gsub(" ", ".", new.names)
  new.names <- gsub("[()]", "", new.names) # Or "\\(|\\)"
  colnames(sales) <- new.names
  rm(old.names)
  rm(new.names)
  
##--------------------------------------------------------------------
##----- Q2 - store registry
##--------------------------------------------------------------------
  
  # Q2.1 - Filter out unique store records and drop store info from main data
  stores <- unique(sales[, c("store.number"
                                 , "store.name"
                                 , "address"
                                 , "store.location"
                                 , "city"
                                 , "zip.code"
                                 , "county"
                                 )
                             ]
                   )
  # Drop store info from main data:
  drop.vars <- c("store.name"
                 , "address"
                 , "county"
                 , "city"
                 , "zip.code"
                 , "store.location"
                 )
  sales[, drop.vars] <- NULL # alternatively sales <- sales[, !names(sales) %in% drop.vars]
  gc()
	rm(drop.vars)
	
  # Q2.2 - Filter out store GPS coordinates from location variable
  coordinates <- strsplit(stores$store.location, "[()]")
  coordinates <- sapply(coordinates, function(x) strsplit(x[2], ", "))
  stores$store.latitude <- as.numeric(sapply(coordinates, function(x) x[1]))
  stores$store.longitude <- as.numeric(sapply(coordinates, function(x) x[2]))
  rm(coordinates)
  
  # Q2.3 - Drop location variable
  stores$store.location <- NULL
  
  # Q2.4 - Average GPS coordinates for stores
  coordinates <- aggregate(cbind(store.latitude, store.longitude) ~ store.number
                           , stores
                           , mean
                           )
  stores <- merge(stores[, !names(stores) %in% c("store.latitude", "store.longitude")]
                   , coordinates
                   , by = c("store.number")
                   , all.x = TRUE
                   )
  rm(coordinates)
  
  # Q2.5 - Removing duplicates
  stores <- unique(stores)
  
  # Q2.6 - Fix address, city and county names
  stores$address <- str_to_title(stores$address)
  stores$address <- gsub("[.,]", "", stores$address)
  stores$city <- str_to_title(stores$city)
  stores$county <- str_to_title(stores$county)
  
  # Q2.7 - Remove duplicates
  stores <- unique(stores)
  
  # Q2.8 - Fill in missing country names
  # We do so by (1) extracting unique combinations of (store.number, county)
  # (2) removing county variable from stores
  # and (3) adding it back from previously selected unique combinations
  stores <- merge(stores[, !names(stores) %in% c("county")]
                   , unique((stores[!is.na(stores$county), c("store.number", "county")]))
                   , by = c("store.number")
                   , all.x = T
                   )
  
  # Q2.9 - Remove duplicates
  stores <- unique(stores)
  
  # Q2.10 - Extract duplicates
  stores$dup <- as.integer(duplicated(stores$store.number) 
  													| duplicated(stores$store.number, fromLast = TRUE)
  													)
  										 
	
  # Q2.11 - Check for proper zip/city/county combinations
  # Import data with correct geo info
  geo <- read.csv(file.choose()
  										 , check.names = FALSE
  										 , stringsAsFactors = FALSE
  										 , na.strings = ""
  										 )
  # Create match variable inside imported data
  geo$match <- 1L
  # Merge stores with geo so that match variable is added to stores
  # but only when stores has correct zipcode/city/county combination
  stores <- merge(stores, geo
  						 , by.x = c("zip.code", "city", "county")
  						 , by.y = c("zipcode", "city", "county")
  						 , all.x = TRUE, all.y = FALSE
  						 )
  # NAs in stores$match correspond to records with wrong geo info
  # Make those records have match = 0
  stores$match[is.na(stores$match)] <- 0
  
  
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
  sales$category <- NA_character_
  sales$category[grepl(' tequila|^tequila', sales$subcategory, ignore.case = TRUE)] <- "Tequila"
  sales$category[grepl(' gin|^gin', sales$subcategory, ignore.case = TRUE)] <- "Gin"
  sales$category[grepl(' brand|^brand', sales$subcategory, ignore.case = TRUE)] <- "Brandy"
  
##--------------------------------------------------------------------
##----- Q4 - export cleaned data
##--------------------------------------------------------------------
  
  write.csv(sales, "sales.csv")
  write.csv(stores, "stores.csv")
  
  