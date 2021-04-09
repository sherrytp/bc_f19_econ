##--------------------------------------------------------------------
##----- General
##--------------------------------------------------------------------
  
  # Clear the workspace
  rm(list = ls()) # Clear environment
  gc()            # Clear memory
  cat("\f")       # Clear the console
  
  # Set working directory and path to data, if needed
  #setwd("")
  
  # Prepare needed libraries
  library.list <- c("stringr", "data.table")
  for (i in 1:length(library.list)) {
    if (!library.list[i] %in% rownames(installed.packages())) {
      install.packages(library.list[i], repos="http://cran.rstudio.com/", dependencies=TRUE)
    }
    library(library.list[i], character.only=TRUE)
  }
  rm(library.list)


##--------------------------------------------------------------------
##----- Q1 - import data and rename variables
##--------------------------------------------------------------------
  
  sales <- fread("../../data/iowa.liquor.r.hw3.csv")
  
  old.names <- colnames(sales)
  new.names <- tolower(old.names)
  new.names <- gsub(" ", ".", new.names)
  new.names <- gsub("[()]", "", new.names)
  setnames(sales, new.names)
  rm(old.names)
  rm(new.names)
  gc()
  
 # Replace empty strings with NAs
  for (i in 1:ncol(sales)) {
    set(sales
        , i = which(sales[[i]] == "") #grep as an alternative
        , j = i
        , value = NA
        )  
  }
  
##--------------------------------------------------------------------
##----- Q2 - store registry
##--------------------------------------------------------------------
  
  # Q2.1 - Filter out unique store records
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
  uniqueN(stores)
  uniqueN(sales[, store.number])
  
  # Q2.2 - Filter out store GPS coordinates from location variable
  stores[, coordinates := tstrsplit(store.location, "[()]", keep = c(2))
           ][, c("store.latitude", "store.longitude") := tstrsplit(coordinates, ", ")
             ][, ':='(store.latitude = as.numeric(store.latitude)
                      , store.longitude = -as.numeric(store.longitude)
                      , coordinates = NULL
                      )
               ]
  
  # Q2.3 - Drop location variable
  stores[, store.location := NULL]
  
  # Q2.4 - Average GPS coordinates for stores
  stores <- stores[, ':='(store.latitude = mean(store.latitude, na.rm = TRUE)
                          , store.longitude = mean(store.longitude, na.rm = TRUE)
                          )
  								 , by = .(store.number)
  								 ]
  
  # Q2.5 - Removing duplicates
  stores <- unique(stores)
  
  # Q2.6 - Fix address, city and county names
  # Get rid of caps:
  stores[, ':=' (county = str_to_title(county)
                     , city = str_to_title(city)
                     , address = str_to_title(address)
                     , store.name = str_to_title(store.name)
                     )
         ]
  # Remove commas, dots, numbers and quotes
  stores[, ':='(address = gsub("[.,]", "", address) # Remove commas, dots
                  , store.name = gsub("#\\d{1,}|# \\d{1,}|\"", "", store.name) # Remove numbers and quotes
                  )
         ]
  # Remove part after /
  stores[, store.name := tstrsplit(store.name, " /", keep = c(1))] # Alternatively gsub("/.*","", store.name)
  # Remove double spaces
  stores[, ':=' (store.name = gsub("  ", " ", store.name)
                         , address = gsub("  ", " ", address)
                         )
         ]
  # Remove trailing/leading spaces
  stores[, ':='(address = gsub("^\\s+|\\s+$", "", address) 
                  , store.name = gsub("^\\s+|\\s+$", "", store.name)
                  )
         ]
  # Unify address shortcuts
  stores[grepl("Avenue", address), address := gsub("Avenue", "Ave", address)
         ][grepl("Road", address), address := gsub("Road", "Rd", address)
           ][grepl("Street", address), address := gsub("Street", "St", address)
             ][grepl("Highway", address), address := gsub("Highway", "Hwy", address)
               ][grepl("West", address), address := gsub("West", "W", address)
                 ][grepl("South", address), address := gsub("South", "S", address)
                   ][grepl("East", address), address := gsub("East", "E", address)
                     ][grepl("North", address), address := gsub("North", "N", address)
                       ]
  
  # Q2.7 - Remove duplicates
  stores <- unique(stores)
  
  # Q2.8 - add zipcode/city/county matches
  geo <- fread("../../data/iowa.geographies.csv")
  geo[, ':='(match = 1L
             , zipcode = as.character(zipcode)
             )
      ]
  stores <- geo[stores, on = c("city", "county", zipcode = "zip.code")]
  stores[is.na(match), match := 0]
  setnames(stores, "zipcode", "zip.code")
  stores[match == 0, .N]
  
  # Q2.9 - remove duplicates with match = 0
  tmp <- unique(stores[match == 1, store.number]) # Stores with match = 1
  stores <- stores[!(store.number %in% tmp & match == 0)] # Filter out stores with match = 0 if they also have match = 1
  rm(tmp)
  stores[match == 0, .N]
                                      
##--------------------------------------------------------------------
##----- Q3 - cleaning sales data
##--------------------------------------------------------------------
  
  # Q3.1 - Convert sales and prices to proper format
  sales[, ':='(state.bottle.retail = as.numeric(gsub("\\$", "", state.bottle.retail))
  						 , sale.dollars = as.numeric(gsub("\\$", "", sale.dollars))
  						 )
  			]
  
  # Q3.2 - Create subcategory variable
  sales[, ":="(subcategory = str_to_title(category.name)
  						 , category.name = NULL
  						 )
  			]
  
  # Q3.3 - Create proper category variable
  sales[, category := NA_character_
  			][grepl(' tequila|^tequila', subcategory, ignore.case = TRUE)
  				, category := "Tequila"
  				][grepl(' gin|^gin', subcategory, ignore.case = TRUE)
  					, category := "Gin"
  					][grepl(' brand|^brand', subcategory, ignore.case = TRUE)
  						, category := "Brandy"]
  
##--------------------------------------------------------------------
##----- Q4 - export cleand data
##--------------------------------------------------------------------
  
  fwrite(sales, "sales.csv")
  fwrite(stores, "stores.csv")
  
  