##--------------------------------------------------------------------
##--------------------------------------------------------------------
##----- MSAE Bootcamp on R, Sep 8, 2018
##----- Part 3: Basic visualizations
##--------------------------------------------------------------------
##--------------------------------------------------------------------

#-------------------------------------------------------------------------
#---- Step 1: prepare the environment, load libraries and import data
#-------------------------------------------------------------------------
  # Wipe the environment clean
  rm(list = ls())
	cat("\f")
  
  # Prepare needed libraries
  tmp.library.list <- c("readxl")
  for (i in 1:length(tmp.library.list)) {
    if (!tmp.library.list[i] %in% rownames(installed.packages())) {
      install.packages(tmp.library.list[i])
    }
    library(tmp.library.list[i], character.only = TRUE)
  }
  rm(tmp.library.list)
  
  # Set working directory
  # setwd("")
  
  # Load the sample orders data that we used for Excel and Tableau
  orders <- read_excel("../data/orders.xlsx") # or you could use file.choose()
  
  # read_excel creates a tiblle as the result, but we rather want a dataframe
  # So we make it to be one:
  orders <- as.data.frame(orders)

  # Our variables have spaces and upper case letters, which maybe confusing
  # Let's fix that
  colnames(orders) <- tolower(gsub(" |-", "."
                                   , colnames(orders)
                                   )
                              )
  
#-------------------------------------------------------------------------
#---- Step 2: R's generic histogram
#-------------------------------------------------------------------------
  # Default histogram
  hist(orders$sales)
  
  # Add more breaks
  hist(orders$sales, breaks = 100)
  
  # Zoom in on x-axis
  hist(orders$sales, breaks = 100, xlim = c(0, 5000))
  
  # Filter out data to use in histogram
  hist(orders$sales[orders$sales < 5000], breaks = 100)
  hist(orders$sales[orders$sales < 3000], breaks = 100)
  hist(orders$sales[orders$sales < 2000], breaks = 50)
  
  # All graphic options for R's default plotting routine
  help("par")
  
  # Add some of those options to make it look nicer
  hist(orders$sales[orders$sales < 2000]
       , breaks = 50         # Number of bars
       , freq = T           # Whether to use counts or probabilities
       , col = "#F0E442"     # Colof of bars' inside fill
       , border = "red"      # Color of bars' outside border
       , density = NULL      # Solid fill, or filled with line pattern if a number is supplied
       , labels = F          # Show counts/frequencies on top of bars
       , main = "Sales distribution"     # Histogram title
       , xlab = "Order value, $"         # X-axis title
       , ylab = ""       # Y-axis title
       , las = 1             # Alignment of labels on the axis
       , font = 1            # Font type for marks 
       , font.lab = 2        # Font type for axis titles
       , font.main = 2       # Font type for the plot title
       , lty = 1            # Line types
       , lwd = 1
       )
  
  # You can save the output of the command, but it only saves numerical values, without extra graphic parameters
  sales.hist <- hist(orders$sales[orders$sales < 2000]
                     , breaks = 50         # Number of bars
                     , freq = T            # Whether to use counts or probabilities
                     , col = "#F0E442"     # Colof of bars' inside fill
                     , border = "red"      # Color of bars' outside border
                     , density = NULL      # Solid fill, or filled with line pattern if a number is supplied
                     , labels = F          # Show counts/frequencies on top of bars
                     , main = "Sales distribution"     # Histogram title
                     , xlab = "Order value, $"         # X-axis title
                     , ylab = "Number of orders"       # Y-axis title
                     , las = 1             # Alignment of labels on the axis
                     , font = 1            # Font type for marks 
                     , font.lab = 2        # Font type for axis titles
                     , font.main = 2       # Font type for the plot title
                     , lty = 1             # Line types
                     , lwd = 1
                     )
  sales.hist
  
  # Plotting saved histogram objects is done with default graphics settings
  plot(sales.hist)
  
  # You can, however, specify those explicitly inside plot() function
  plot(sales.hist
       , col = "#F0E442"     # Colof of bars' inside fill
       , border = "red"      # Color of bars' outside border
       , density = NULL      # Solid fill, or filled with line pattern if a number is supplied
       , labels = F          # Show counts/frequencies on top of bars
       , main = "Sales distribution"     # Histogram title
       , xlab = "Order value, $"         # X-axis title
       , ylab = ""       # Y-axis title
       , las = 1             # Alignment of labels on the axis
       , font = 1            # Font type for marks 
       , font.lab = 2        # Font type for axis titles
       , font.main = 2       # Font type for the plot title
       , lty = 1             # Line types
       , lwd = 1
       )
  
  #To export the picture automatically you need to open png() device
  png(file = "sales.hist.png", width = 1920, height = 1200)
    # Then run the command for plotting
    hist(orders$sales[orders$sales < 2000]
         , breaks = 50         # Number of bars
         , freq = T            # Whether to use counts or probabilities
         , col = "#F0E442"     # Colof of bars' inside fill
         , border = "red"      # Color of bars' outside border
         , density = NULL      # Solid fill, or filled with line pattern if a number is supplied
         , labels = F          # Show counts/frequencies on top of bars
         , main = "Sales distribution"     # Histogram title
         , xlab = "Order value, $"         # X-axis title
         , ylab = "Number of orders"       # Y-axis title
         , las = 1             # Alignment of labels on the axis
         , font = 1            # Font type for marks 
         , font.lab = 2        # Font type for axis titles
         , font.main = 2       # Font type for the plot title
         , lty = 1             # Line types
         , lwd = 1
         )
  # And the clost the png() device
  dev.off()
  
  # Use "res = " option to control for element sizes in higher resolutions
  png(file = "sales.hist.png"
      , width = 1920
      , height = 1200
      , res = 216
      )
    hist(orders$sales[orders$sales < 2000]
         , breaks = 50         # Number of bars
         , freq = T            # Whether to use counts or probabilities
         , col = "#F0E442"     # Colof of bars' inside fill
         , border = "red"      # Color of bars' outside border
         , density = NULL      # Solid fill, or filled with line pattern if a number is supplied
         , labels = F          # Show counts/frequencies on top of bars
         , main = "Sales distribution"     # Histogram title
         , xlab = "Order value, $"         # X-axis title
         , ylab = ""       # Y-axis title
         , las = 1             # Alignment of labels on the axis
         , font = 1            # Font type for marks 
         , font.lab = 2        # Font type for axis titles
         , font.main = 2       # Font type for the plot title
         , lty = 1             # Line types
         , lwd = 1
         )
  dev.off()

#-------------------------------------------------------------------------
#---- Step 3: R's generic plotting
#-------------------------------------------------------------------------
  
  # Basis scatter plot
  plot(order$sales) # doesn't work because only one dimension
  plot(orders$sales, orders$profit)
  
  # Adjust some of the visual options
  plot(orders$sales, orders$profit
       , type = "p"          # "p" for points
       , pch = 21             # Point types
       , col = "#002784"     # Main color of plot elements
       , bg = "red"          # Background or fill color
       , main = "Sales vs profits"     # Histogram title
       , xlab = "Sales, $"         # X-axis title
       , ylab = "Profits, $"       # Y-axis title
       , xlim = c(0, 3000)         # Zoom in along x-axis
       , ylim = c(quantile(orders$profit, 0.025)
                  , quantile(orders$profit, 0.975)
                  ) # Zoom in along y-axis
       , las = 1             # Alignment of labels on the axis
       , font = 1            # Font type for marks 
       , font.lab = 2        # Font type for axis titles
       , font.main = 2       # Font type for the plot title
       )
  
  # Export graph
  png(file = "sales.vs.profits.png"
      , width = 1920
      , height = 1200
      , res = 216
      )
    plot(orders$sales[orders$sales < 2000], orders$profit[orders$sales < 2000]
         , type = "p"          # "p" for points
         , pch = 21             # Point types
         , col = "#002784"     # Main color of plot elements
         , bg = "red"          # Background or fill color
         , density = NULL      # Solid fill, or filled with line pattern if a number is supplied
         , main = "Sales vs profits for orders less that $2000"     # Histogram title
         , xlab = "Sales, $"         # X-axis title
         , ylab = "Profits, $"       # Y-axis title
         , las = 1             # Alignment of labels on the axis
         , font = 1            # Font type for marks 
         , font.lab = 2        # Font type for axis titles
         , font.main = 2       # Font type for the plot title
    )
  dev.off()
  
  # Multiple datasets on one graph
  plot(orders$sales[orders$sales < 2000 
                    & orders$category == "Technology"]
       , orders$profit[orders$sales < 2000 
                       & orders$category == "Technology"]
       , type = "p"          # "p" for points
       , pch = 21             # Point types
       , col = "#002784"     # Main color of plot elements
       , bg = "blue"          # Background or fill color
       , density = NULL      # Solid fill, or filled with line pattern if a number is supplied
       , main = "Sales vs profits for orders less that $2000"     # Histogram title
       , xlab = "Sales, $"         # X-axis title
       , ylab = "Profits, $"       # Y-axis title
       , las = 1             # Alignment of labels on the axis
       , font = 1            # Font type for marks 
       , font.lab = 2        # Font type for axis titles
       , font.main = 2       # Font type for the plot title
       )
    
 		# Adds extra layer of plot objects to the last plot
    points(orders$sales[orders$sales < 2000 
                        & orders$category == "Furniture"]
          , orders$profit[orders$sales < 2000 
                          & orders$category == "Furniture"]
          , pch = 22             # Point types
          , col = "#002784"     # Main color of plot elements
          , bg = "yellow"          # Background or fill color
          , density = NULL      # Solid fill, or filled with line pattern if a number is supplied
          )
    
    # Adds extra layer of plot objects to the last plot
    points(orders$sales[orders$sales < 2000 
                        & orders$category == "Office Supplies"]
           , orders$profit[orders$sales < 2000 
                           & orders$category == "Office Supplies"]
           , pch = 23             # Point types
           , col = "#002784"     # Main color of plot elements
           , bg = "green"          # Background or fill color
           , density = NULL      # Solid fill, or filled with line pattern if a number is supplied
           )
    
    # Add legend 
    # See more at http://www.sthda.com/english/wiki/add-legends-to-plots-in-r-software-the-easiest-way)
    legend("topleft" # Positioning could be in (x,y) coordinates or in terms like "bottomright"
           , legend = c("Technology", "Furniture", "Office Supplies")
           , col = c("#002784", "#002784", "#002784")
           #, fill = c("blue", "yellow", "green")
           , pch = c(21, 22, 23)
           , pt.bg = c("blue", "yellow", "green")
           , inset = 0
           , bg = "white" # Background of legend box
           , bty = "n" # Whether or not to draw a box around legend
           #, box.lty = 0 # Border line type for legend box
           #, box.lwd = 0 # Border line width for legend box
           )
  
  
  # You can also plot multiple graphs side by side using par() function
  # See more at https://www.statmethods.net/advgraphs/layout.html
  par(mfrow = c(1,3)) 
  hist(orders$sales[orders$sales < 2000 
                    & orders$category == "Technology"]
       , breaks = 50         # Number of bars
       , freq = T            # Whether to use counts or probabilities
       , col = "#F0E442"     # Colof of bars' inside fill
       , border = "red"      # Color of bars' outside border
       , density = NULL      # Solid fill, or filled with line pattern if a number is supplied
       , labels = F          # Show counts/frequencies on top of bars
       , main = "Sales distribution, Technology"     # Histogram title
       , xlab = "Order value, $"         # X-axis title
       , ylab = ""       # Y-axis title
       , las = 1             # Alignment of labels on the axis
       , font = 1            # Font type for marks 
       , font.lab = 2        # Font type for axis titles
       , font.main = 2       # Font type for the plot title
       , lty = 1             # Line types
       , lwd = 1
       )
  hist(orders$sales[orders$sales < 2000 
                    & orders$category == "Furniture"]
       , breaks = 50         # Number of bars
       , freq = T            # Whether to use counts or probabilities
       , col = "#F0E442"     # Colof of bars' inside fill
       , border = "red"      # Color of bars' outside border
       , density = NULL      # Solid fill, or filled with line pattern if a number is supplied
       , labels = F          # Show counts/frequencies on top of bars
       , main = "Sales distribution, Furniture"     # Histogram title
       , xlab = "Order value, $"         # X-axis title
       , ylab = ""       # Y-axis title
       , las = 1             # Alignment of labels on the axis
       , font = 1            # Font type for marks 
       , font.lab = 2        # Font type for axis titles
       , font.main = 2       # Font type for the plot title
       , lty = 1             # Line types
       , lwd = 1
       )
  hist(orders$sales[orders$sales < 2000 
                    & orders$category == "Office Supplies"]
       , breaks = 50         # Number of bars
       , freq = T            # Whether to use counts or probabilities
       , col = "#F0E442"     # Colof of bars' inside fill
       , border = "red"      # Color of bars' outside border
       , density = NULL      # Solid fill, or filled with line pattern if a number is supplied
       , labels = F          # Show counts/frequencies on top of bars
       , main = "Sales distribution, Office Supplies"     # Histogram title
       , xlab = "Order value, $"         # X-axis title
       , ylab = ""       # Y-axis title
       , las = 1             # Alignment of labels on the axis
       , font = 1            # Font type for marks 
       , font.lab = 2        # Font type for axis titles
       , font.main = 2       # Font type for the plot title
       , lty = 1             # Line types
       , lwd = 1
       )
  
  
  