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
  rm(library.list, i)


##--------------------------------------------------------------------
##----- Q1 - import data and rename variables
##--------------------------------------------------------------------
  
  # Import data
  sales <- fread(file.choose())
  
  # Rename variables
  old.names <- colnames(sales)
  new.names <- tolower(old.names)
  new.names <- gsub(" |-", ".", new.names)
  new.names <- gsub("[()]", "", new.names)
  setnames(sales, old.names, new.names)
  # data.table never sets or uses the row names!! 
  
  # Replace empty strings with NAs, using set()
  for (i in 1:ncol(sales)) { 
    set(sales, which(sales[[i]] == ""), i, value = NA)
  }
  
  # Alternatively, using gsub()
  #sapply(sales, gsub(" |-", NA, sales[[i]] == ""))
  rm(i, new.names, old.names)
##--------------------------------------------------------------------
##----- Q2 - store registry
##--------------------------------------------------------------------
  # Q2.1 - Filter out unique store records
  stores <- unique(sales[, .(store.number, store.name, address, store.location, city, zip.code, county)])
  sales[, c("store.number", "store.name", "address", "store.location", "city", "zip.code", "county") := NULL]

  # Q2.2 - Filter out store GPS coordinates from location variable
  location <- tstrsplit(stores[,store.location], "[()]")
  location <- tstrsplit(location[[2]], ",")
  stores[, ':='(store.latitude = location[[1]]       # Can leave comments after each line
                , store.longitude = location[[2]]  # Can leave comments after each line
  )]
  rm(location)

  # Q2.3 - Drop location variable
  stores[, "store.location" := NULL]
  
  # Q2.4 - Average GPS coordinates for stores
  stores[, ':=' (store.latitude = as.numeric(store.latitude))]
  stores[, ':=' (store.longitude = as.numeric(store.longitude))]
  stores <- stores[, ':=' (store.latitude = mean(store.latitude, na.rm = TRUE)
                           , store.longitude = mean(store.longitude, na.rm = TRUE)
                           ) 
                   , by = .(store.number)
                   ]

  # Q2.5 - Removing duplicates
  stores <- unique(stores)
  
  # Q2.6 - Fix address, city and county names
  # apply str_to_title()
  stores[, c("address", "city", "county", "store.name")
         := lapply(.SD, str_to_title)
         , .SDcols = c("address"
                       , "city", "county", "store.name")
         ]
  # remove dots and commas 
  stores[, address := gsub("[.,]", "", address)
         ][, address := gsub("  ", "", address)]

  # remove pound numbers and double quotes, double spaces 
  stores[, store.name := gsub("#\\d{1,}|# \\d{1,}|\"","",store.name)
         ][, store.name := gsub("/.*", "", store.name)
           ][, store.name := gsub("  ","", store.name)]
  
  # remove leading/trailing spaces 
  stores[, c("store.name", "address") 
         := lapply(.SD, function(x) 
           trimws(x, which = c("both"))
           ), .SDcols = c("store.name"
                 ,"address")
         ]  # Alternatively, 
  #stores[, ':='(address = gsub("^\\s+|\\s+$", "", address), store.name = gsub("^\\s+|\\s+$", "", store.name))]

  # remove common abbrevs
  stores[grepl("Avenue", address), address := gsub("Avenue", "Ave", address)
         ][grepl("Road", address), address := gsub("Road", "Rd", address)
           ][grepl("Street", address), address := gsub("Street", "St", address)
             ][grepl("Highway", address), address := gsub("Highway", "Hwy", address)
               ][grepl("West", address), address := gsub("West", "W", address)
                 ][grepl("South", address), address := gsub("South", "S", address)
                   ][grepl("East", address), address := gsub("East", "E", address)
                     ][grepl("North", address), address := gsub("North", "N", address)]
  
  # Q2.7 - Remove duplicates
  stores <- unique(stores)   # why get 2400 not 2261?? 
  
  # Q2.8 - add zipcode/city/county matches - having problems???????
  geo <- fread(file.choose())
  geo[, ':=' (match = 1L, zipcode = as.character(zip.code))]
  stores$zip.code <- as.integer(stores$zip.code)
  stores <- geo[stores, on = .(city, zip.code, county)]

  # Q2.9 - remove duplicates with match = 0
  stores[is.na(match), ':=' (match = 0) ] 
  stores <- stores[match == 1]  # should use unique()? 
  
##--------------------------------------------------------------------
##----- Q3 - cleaning sales data
##--------------------------------------------------------------------

  # Q3.1 - Convert sales and prices to proper format
  sales[, c("state.bottle.retail", "sale.dollars") 
        := lapply(.SD, function(x) as.numeric(gsub("\\$", "", x)))
        , .SDcol = c("state.bottle.retail", "sale.dollars")
        ]
  
  # Q3.2 - Create subcategory variable
  sales[, ':=' (subcategory = category.name
        , category.name = NULL)
        ]
  
  # Q3.3 - Create proper category variable
  sales[, category := NA_character_
        ][grepl(' tequila|^tequila', subcategory, ignore.case = TRUE)
          , category := "Tequila"
          ][grepl(' gin|^gin', subcategory, ignore.case = TRUE)
            , category := 'Gin'
            ][grepl(' brand|^brand', subcategory, ignore.case = TRUE)
              , category := 'Brandy']
  
##--------------------------------------------------------------------
##----- Q4 - export cleand data
##--------------------------------------------------------------------
  fwrite(sales, "sales.csv")
  fwrite(stores, "stores.csv")  
  
  