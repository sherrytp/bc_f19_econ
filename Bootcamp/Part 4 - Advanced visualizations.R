#-------------------------------------------------------------------------
#------ Advanced plotting in R using ggplot
#------ http://r-statistics.co/Complete-Ggplot2-Tutorial-Step1-With-R-Code.html
#------ http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html
#------ http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
#---- Step 1: prepare the environment, load libraries and import data
#-------------------------------------------------------------------------
  # Wipe the environment clean
  rm(list = ls())
  
  # Prepare needed libraries
  library.list <- c("readxl"      # For data import
  									, "ggplot2"   # Main package
  									, "scales"    # For managing chart axis labels/values/scale
  									, "lemon"     # Extra things for ggplot
  									, "gridExtra" # Arrange different plots in a grid
  									)
  for (i in 1:length(library.list)) {
    if (!library.list[i] %in% rownames(installed.packages())) {
      install.packages(library.list[i])
    }
    library(library.list[i], character.only = TRUE)
  }
  rm(library.list)
  
  # Set working directory
  #setwd("")
  
  # Load the sample orders data that we used for Excel and Tableau, but only keep sales < $2000
  orders <- read_excel(file.choose())
  
  # Our variables have spaces, dashes and upper case letters, which maybe confusing
  # Let's fix that
  colnames(orders) <- tolower(gsub(" |-", ".", colnames(orders)))
  
  # Keep only observations with sales < 2000 (to ignore outliers)
  orders <- orders[orders$sales < 2000, ]
  
  # And this time randomly select and keep only 10% of the data (to speed up graphs)
  orders <- orders[sample(1:nrow(orders), 0.1*nrow(orders)), ]
  
#-------------------------------------------------------------------------
#---- Step 2: basics of ggplot
#-------------------------------------------------------------------------
  # Basic call to ggplot function produces empty plot
  ggplot(orders, aes(x = sales, y = profit))
  
  # Add a basic scatter plot
  ggplot(orders, aes(x = sales, y = profit)) +
    geom_point()
  
  # You can save ggplot output as a valid graphics object
  sales.scatter <- ggplot(orders, aes(x = sales, y = profit)) + 
  									geom_point() + geom_smooth()
  sales.scatter <- ggplot(orders, aes(x = sales, y = profit)) + 
                    geom_point()
  
  # And then simply call it to produce output
  sales.scatter
  
  # You can also add features/mappings/aesthetics to it
  sales.scatter + geom_smooth()
  
  # Delete some of the points outside limits
  sales.scatter + 
    geom_smooth() + 
    xlim(c(0, 1000)) + 
    ylim(c(-2000, 2000))
  # Or simply zoom (this retains all points)
  sales.scatter + 
    geom_smooth() + 
    coord_cartesian(xlim=c(0, 1000)
                    , ylim=c(-2000, 2000)
                    )

  # For large datasets you should subset first, and then call ggplot
  # Otherwise ggplot will take longer time to render
  ggplot(orders[orders$sales < 1000, ]
         , aes(x = sales, y = profit)
         ) + 
    geom_point() +
    geom_smooth()
  
  # Add title and axis lables
  ggplot(orders, aes(x = sales, y = profit)) + 
    geom_point() +
    geom_smooth() + 
    labs(title = "Sales vs. Profits"
         , subtitle = "for orders less than $2000"
         , x = "Total sale, $"
         , y = "Total profit, $"
         )
  
  # Customize the looks of pglot elements
  ggplot(orders, aes(x = sales, y = profit)) + 
    geom_point(color = "steelblue"    # Color of points' outline
               , fill = "yellow"     # Color of points' fill
               , shape = 22         # Shape of points, similar to R's default "pch" parameter
               , size = 2           # Size of points
               , alpha = 1          # Transparency of points
               ) +
    geom_smooth(color = "firebrick"
                , weight = 5        # line thickness
                ) + 
    labs(title = "Sales vs. Profits"
         , subtitle = "for orders less than $2000"
         , x = "Total sale, $"
         , y = "Total profit, $"
         )
  
  # You could use some of the built-in themes for customization of looks
  ggplot(orders, aes(x = sales, y = profit)) +
    geom_point(color = "steelblue"    # Color of points' outline
               , fill = "yellow"     # Color of points' fill
               , shape = 22         # Shape of points, similar to R's default "pch" parameter
               , size = 2           # Size of points
               , alpha = 1          # Transparency of points
               ) +
    geom_smooth(color = "firebrick"
                , weight = 5
                , linetype = "solid" # Dashed, dotted, etc.
                ) + 
    labs(title = "Sales vs. Profits"
         , subtitle = "for orders less than $2000"
         , x = "Total sale, $"
         , y = "Total profit, $"
         ) +
    theme_bw()
  
  # Or you could change theme elements yourself
  ggplot(orders, aes(x = sales, y = profit)) +
    geom_point(color = "steelblue"    # Color of points' outline
               , fill = "yellow"     # Color of points' fill
               , shape = 22         # Shape of points, similar to R's default "pch" parameter
               , size = 2           # Size of points
               , alpha = 1          # Transparency of points
               ) +
    labs(title = "Sales vs. Profits"
         , subtitle = "for orders less than $2000"
         , x = "Total sale, $"
         , y = "Total profit, $ \n"
         ) +
    theme_bw() + # Lines below inside theme() will overwrite Steps of theme_bw()
    theme(plot.title = element_text(hjust = 0.5          # Title alignment, 0 - left, 1 - right
                                    , face = "bold"      # font face - bold, italic, etc.
                                    , size = 20          # font size
                                    , color = "#912600") # font color
          , plot.subtitle = element_text(hjust = 0.5     # Horizontal alignment, 0.5 = center
          															 , face = "bold" # Bold font
          															 , size = 14     # Font size
          															 , color = "#912600"  # Font color
          															 )
          , axis.title.x = element_text(face = "bold"
          															, color = "#912600"
          															, size = 14
          															)
          , axis.text.x  = element_text(face = "bold"
          															, vjust = 0.5  # Vertical alignment, 0.5 = middle
          															, size = 12
          															)
          , axis.title.y = element_text(face = "bold"
          															, color = "#912600"
          															, size = 14
          															)
          , axis.text.y  = element_text(face = "bold"
          															, vjust = 0.5
          															, size = 12
          															)
          )
  
  # You could also use variable color/shape/fill mapping inside aes()
  ggplot(orders
  			 , aes(x = sales
         			, y = profit
         			, color = category
         			, shape = market
         			)
         ) +
    geom_point(size = 2           # Size of points
               , alpha = 1          # Transparency of points
               ) +
    labs(title = "Sales vs. Profits"
         , subtitle = "for orders less than $2000"
         , x = "Total sale, $"
         , y = "Total profit, $"
         ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5
    																, face = "bold"
    																, size = 20
    																, color = "#912600"
    																)
          , plot.subtitle = element_text(hjust = 0.5
          															 , face = "bold"
          															 , size = 14
          															 , color = "#912600"
          															 )
          , axis.title.x = element_text(face = "bold"
          															, color = "#912600"
          															, size = 14
          															)
          , axis.text.x  = element_text(face = "bold"
          															, vjust = 0.5
          															, size = 12
          															)
          , axis.title.y = element_text(face = "bold"
          															, color = "#912600"
          															, size = 14
          															)
          , axis.text.y  = element_text(face = "bold"
          															, vjust = 0.5
          															, size = 12
          															)
          )
  
  # Now you have a legend, which you can also customize inside theme()
  sales.scatter <- ggplot(orders
                         , aes(x = sales
                               , y = profit
                               , color = category
                               , shape = market
                         			)
                         ) +
    geom_point(size = 2           # Size of points
               , alpha = 1          # Transparency of points
               ) +
    labs(title = "Sales vs. Profits"
         , subtitle = "for orders less than $2000"
         , x = "Total sale, $"
         , y = "Total profit, $"
         ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5
    																, face = "bold"
    																, size = 20
    																, color = "#912600"
    																)
          , plot.subtitle = element_text(hjust = 0.5
          															 , face = "bold"
          															 , size = 14
          															 , color = "#912600"
          															 )
          , axis.title.x = element_text(face = "bold"
          															, color = "#912600"
          															, size = 14
          															)
          , axis.text.x  = element_text(face = "bold"
          															, vjust = 0.5
          															, size = 12
          															)
          , axis.title.y = element_text(face = "bold"
          															, color = "#912600"
          															, size = 14
          															)
          , axis.text.y  = element_text(face = "bold"
          															, vjust = 0.5
          															, size = 12
          															)
          , legend.text = element_text(face = "bold"
          														 , size = 12
          														 )
          , legend.position = c(0.1, 0.2)    # Or use "bottom", "top", "left", "right"
          , legend.key.size = unit(1, 'lines') # Size of elements inside legend box
          , legend.title = element_blank()    # Hides legend title
          , legend.background = element_rect(size = 0.5
          																	 , linetype = "solid"
          																	 , color = "black"
          																	 )
          )
  sales.scatter
  
  # Export the result
  png(file = "sales.scatter.png"
      , width = 1920
      , height = 1200
      , res = 148
      )
    sales.scatter
  dev.off()
  
#-------------------------------------------------------------------------
#---- Step 3: histogram
#-------------------------------------------------------------------------
  # Histogram 
  # See more at http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
  ggplot(orders, aes(x = sales)) +
    geom_histogram()
  
  # Change the number of bins
  ggplot(orders, aes(x = sales)) +
    geom_histogram(bins = 50)
  # Or change the width of bins
  ggplot(orders, aes(x = sales)) +
    geom_histogram(binwidth = 100)
  
  # To make histograms of different variables compararable, one needs to use % in y axis
  # It will also help to have more ticks on x-axis
  ggplot(orders, aes(x = sales)) +
    geom_histogram(binwidth = 50
    							 , aes(y = ..count../sum(..count..)) # ..count../sum(..count..) is an internal ggplot object
    							 ) + 
  	scale_y_continuous(labels = percent # This is where the package "scales" gets used first time
  										 ) +
  	scale_x_continuous(breaks = seq(from = 0        # Adjust x-axis ticks
  																	, to = 2000
  																	, by = 200
  																	)
  										 , labels = seq(from = 0      # Adjust labels for ticks
  																	, to = 2000
  																	, by = 200
  																	)
  										 )
  
  # Add custom lines and text to the chart
  ggplot(orders, aes(x = sales)) +
    geom_histogram(binwidth = 50
    							 , aes(y = ..count../sum(..count..))
    							 ) + 
  	scale_y_continuous(labels = percent
  										 , limits = c(0, 0.4)
  										 ) +
  	scale_x_continuous(breaks = seq(from = 0        # Adjust x-axis ticks
  																	, to = 2000
  																	, by = 200
  																	)
  										 , labels = seq(from = 0      # Adjust labels for ticks
  																	, to = 2000
  																	, by = 200
  																	)
  										 ) +
  	geom_vline(aes(xintercept = mean(sales)) # Adds a vertical line
  						 ) +
  	geom_vline(aes(xintercept = median(sales))
  						 ) +
  	annotate(geom = "text"   # Adds a text box
  					 , x = mean(orders$sales) + 100
  					 , y = 0.35
  					 , label = paste0("Mean = "
  					                  , round(mean(orders$sales), 2)
  					                  )
  					 ) +
    annotate(geom = "text"
    				 , x = median(orders$sales) - 100
    				 , y = 0.35
             , label = paste0("Median = "
                              , round(median(orders$sales),2)
                              )
    				 ) 
  
  # Adjust the looks
  sales.hist <- ggplot(orders, aes(x = sales)) +
    geom_histogram(bins = 50
    							 , aes(y = ..count../sum(..count..))
                   , color = "darkblue"
                   , fill = "lightblue"
                   ) +
  	scale_y_continuous(labels = percent
  										 , limits = c(0, 0.4)
  										 ) +
  	scale_x_continuous(breaks = seq(from = 0        # Adjust x-axis ticks
  																	, to = 2000
  																	, by = 200
  																	)
  										 , labels = seq(from = 0      # Adjust labels for ticks
  																	, to = 2000
  																	, by = 200
  																	)
  										 ) +
  	geom_vline(aes(xintercept = mean(sales))
  						 , color = "red"
  						 , linetype = "dashed"
  						 , size = 1.2
  						 ) +
  	geom_vline(aes(xintercept = median(sales))
  						 , color = "dark green"
  						 , linetype = "dashed"
  						 , size = 1.2
  						 ) +
  	annotate(geom = "text"
  					 , x = mean(orders$sales) + 100
  					 , y = 0.35
  					 , color = "red"
  					 , size = 4
  					 , fontface = "bold"
  					 , label = paste0("Mean\n=\n"
  					                  , round(mean(orders$sales), 2)
  					                  )
  					 , lineheight = 0.75  # Reduce line spacing
  					 ) +
    annotate(geom = "text"
    				 , x = median(orders$sales) - 100
    				 , y = 0.35
             , color = "dark green"
    				 , size = 4
    				 , fontface = "bold"
             , label = paste0("Median\n=\n"
                              , round(median(orders$sales),2)
                              )
    				 , lineheight = 0.75  # Reduce line spacing
    				 ) + 
    labs(title = "Sales Distribution"
         , subtitle = "for orders less than $2000"
         , x = "Total sale, $"
         , y = "Number of orders"
         ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5
    																, face = "bold"
    																, size = 20
    																, color = "#912600"
    																)
          , plot.subtitle = element_text(hjust = 0.5
          															 , face = "bold"
          															 , size = 14
          															 , color = "#912600"
          															 )
          , axis.title.x = element_text(face = "bold"
          															, color = "#912600"
          															, size = 14
          															)
          , axis.text.x  = element_text(face = "bold"
          															, vjust = 0.5
          															, size = 12
          															)
          , axis.title.y = element_text(face = "bold"
          															, color = "#912600"
          															, size = 14
          															)
          , axis.text.y  = element_text(face = "bold"
          															, vjust = 0.5
          															, size = 12
          															)
          )
  sales.hist
  
  # Add a density curve to a histogram
  sales.hist + 
    geom_density(aes(y = ..density..))
  # That doesn't look right because our histogram and density use different y scale
  # So, we need to change hist to have density
  sales.hist <- ggplot(orders, aes(x = sales)) +
    geom_histogram(bins = 50
    							 , aes(y = ..density..) # Here we now have density
                   , color = "darkblue"
                   , fill = "lightblue"
                   ) +
    geom_density(aes(y = ..density..)
                 , color = "darkblue"
                 , size = 1.2
                 ) + 
  	# scale_y_continuous() +
  	scale_x_continuous(breaks = seq(from = 0        
  																	, to = 2000
  																	, by = 200
  																	)
  										 , labels = seq(from = 0      
  																	, to = 2000
  																	, by = 200
  																	)
  										 ) +
  	geom_vline(aes(xintercept = mean(sales))
  						 , color = "red"
  						 , linetype = "dashed"
  						 , size = 1.2
  						 ) +
  	geom_vline(aes(xintercept = median(sales))
  						 , color = "dark green"
  						 , linetype = "dashed"
  						 , size = 1.2
  						 ) +
  	annotate(geom = "text"
  					 , x = mean(orders$sales) + 100
  					 , y = 0.0075 # This needs to be changed to accomodate new scale
  					 , color = "red"
  					 , size = 4
  					 , fontface = "bold"
  					 , label = paste0("Mean\n=\n"
  					                  , round(mean(orders$sales), 2)
  					                  )
  					 , lineheight = 0.75  # Reduce line spacing
  					 ) +
    annotate(geom = "text"
    				 , x = median(orders$sales) - 100
    				 , y = 0.0075 # This needs to be changed to accomodate new scale
             , color = "dark green"
    				 , size = 4
    				 , fontface = "bold"
             , label = paste0("Median\n=\n"
                              , round(median(orders$sales),2)
                              )
    				 , lineheight = 0.75  # Reduce line spacing
    				 ) + 
    labs(title = "Sales Distribution"
         , subtitle = "for orders less than $2000"
         , x = "Total sale, $"
         , y = ""
         ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5
    																, face = "bold"
    																, size = 20
    																, color = "#912600"
    																)
          , plot.subtitle = element_text(hjust = 0.5
          															 , face = "bold"
          															 , size = 14
          															 , color = "#912600"
          															 )
          , axis.title.x = element_text(face = "bold"
          															, color = "#912600"
          															, size = 14
          															)
          , axis.text.x  = element_text(face = "bold"
          															, vjust = 0.5
          															, size = 12
          															)
          , axis.title.y = element_text(face = "bold"
          															, color = "#912600"
          															, size = 14
          															)
          , axis.text.y  = element_text(face = "bold"
          															, vjust = 0.5
          															, size = 12
          															)
          )
  sales.hist
  
  # We can also add a normal distribution density after creating the ggplot!!! 
  sales.hist +
    stat_function(fun = dnorm
                  , n = nrow(orders)
                  , args = list(mean = mean(orders$sales), sd = sd(orders$sales))
                  , color = "purple"
                  , size = 1.2
                  ) +
    annotate(geom = "text"
    				 , x = 400
    				 , y = 0.0015
             , color = "purple"
    				 , size = 4
    				 , fontface = "bold"
             , label = "Normal denstity"
    				 , lineheight = 0.75  # Reduce line spacing
    				 , hjust = 0 # Make text be on the left of x position
    				 )
  
  
  # Multiple histograms don't fill well on one graph
  ggplot(orders, aes(x = sales
                     #, color = category # this doesn't work for bars
                     #, shape = category # this doesn't work either
  									 , fill = category  # Using category to define fill color
  									 )
  			 ) +
    geom_histogram(binwidth = 50
    							 , aes(y = ..count../sum(..count..))
    							 , color = "darkblue"    # Border color of bars is the same for everyone
    							 #, position = "dodge"   # This is even worse
    							 ) + 
  	scale_y_continuous(labels = percent) +
  	scale_x_continuous(breaks = seq(from = 0        # Adjust x-axis ticks
  																	, to = 2000
  																	, by = 200
  																	)
  										 , labels = seq(from = 0      # Adjust labels for ticks
  																	, to = 2000
  																	, by = 200
  																	)
  										 ) +
  	theme_bw()
  
  # So it's better to put them in a facet grid
  ggplot(orders, aes(x = sales
  									 , fill = category
  									 )
  			 ) +
    geom_histogram(binwidth = 50
    							 , aes(y = ..count../sum(..count..))
    							 , color = "darkblue"
    							 ) + 
  	scale_y_continuous(labels = percent) +
  	scale_x_continuous(breaks = seq(from = 0        # Adjust x-axis ticks
  																	, to = 2000
  																	, by = 200
  																	)
  										 , labels = seq(from = 0      # Adjust labels for ticks
  																	, to = 2000
  																	, by = 200
  																	)
  										 ) +
  	facet_wrap( ~ category
  							, scale = "fixed"  # Use "free" if same scale is not good
  							, ncol = 1
  							, nrow = 3
  							, strip.position = "bottom") +
  	theme_bw() + 
  	theme(strip.text.x = element_text(size = 12
  																		, face = "bold"
  																		)
  				, strip.background = element_blank() # No background color
  				, strip.placement = "outside"
  				, legend.position = "none"           # Don't show legend, since we have facet labels
  				)
  
  
#-------------------------------------------------------------------------
#---- Step 4: Box plot
#-------------------------------------------------------------------------  
    
  # Box plot, vertical
  ggplot(orders, aes( x = category
                    , y = sales
  									 , fill = category
  									 )
  			 ) +
    geom_boxplot() + 
  	theme_bw()
  
  # Box plot, horizontal
  ggplot(orders, aes(x = category
  									 , y = sales
  									 , fill = category
  									 )
  			 ) +
    geom_boxplot() + 
  	coord_flip() +  # Flip axis around, can be used in any ggplot object (note that you can't switch x and y in aes())
  	theme_bw()
  
  # Add another dimension
  ggplot(orders, aes(x = order.priority
  									 , y = sales
  									 , fill = category
  									 )
  			 ) +
    geom_boxplot() + 
  	theme_bw() 
  
  # Priority is in random order because it's a character variable
  # We need to make it an ordered factor for ggplot to properly use it
  orders$order.priority <- factor(orders$order.priority
  																, levels = c("Low"
  																						 , "Medium"
  																						 , "High"
  																						 , "Critical"
  																						 )
  																, ordered = TRUE
  																)
  ggplot(orders, aes(x = order.priority
  									 , y = sales
  									 , fill = category
  									 )
  			 ) +
    geom_boxplot() + 
  	theme_bw() 
  
  # Customize
   ggplot(orders, aes(x = order.priority
  									 , y = sales
  									 , fill = category
  									 )
  			 ) +
    geom_boxplot(alpha = 0.5 # Transparancy of fill color, 1 = none
    						 , color = "black" # Border color of boxes
    						 , size = 0.5      # Border size
    						 ) +
   	scale_fill_manual(values = c("dark orange"     # This allows to use your own fill colors
   															 , "dark green"
   															 , "dark blue"
   															 )
   										) +
   	scale_y_continuous(breaks = seq(from = 0        # Adjust x-axis ticks
  																	, to = 2000
  																	, by = 200
  																	)
  										 , labels = seq(from = 0      # Adjust labels for ticks
  																	, to = 2000
  																	, by = 200
  																	)
  										 ) + 
   	labs(title = "Sales vs order priority"
   			 , subtitle = "for orders less than $2000"
         , x = "Order priority"
         , y = "Sales, $"
        ) +
   	coord_flip() + 
  	theme_bw() + 
   	theme(plot.title = element_text(hjust = 0.5
    																, face = "bold"
    																, size = 20
    																, color = "#912600"
    																)
          , plot.subtitle = element_text(hjust = 0.5
          															 , face = "bold"
          															 , size = 14
          															 , color = "#912600"
          															 )
          , axis.title.x = element_text(face = "bold"
          															, color = "#912600"
          															, size = 14
          															)
          , axis.text.x  = element_text(face = "bold"
          															, vjust = 0.5
          															, size = 12
          															)
          , axis.title.y = element_text(face = "bold"
          															, color = "#912600"
          															, size = 14
          															)
          , axis.text.y  = element_text(face = "bold"
          															, vjust = 0.5
          															, size = 12
          															)
          )
  
#-------------------------------------------------------------------------
#---- Step 5: Bar chart
#-------------------------------------------------------------------------    
  
   # Stacked bars, absolute values
   ggplot(orders, aes(x = market
   									 , y = sales/1000
   									 , fill = category
   									 )
   			 ) + 
   	geom_bar(stat = "identity"
   					 , position = "stack"
   					 )
   
   # Stacked bars, relative values
   ggplot(orders, aes(x = market
   									 , y = sales/1000
   									 , fill = category
   									 )
   			 ) + 
   	geom_bar(stat = "identity"
   					 , position = "fill"
   					 )
   
   # Non-stacked bars
   ggplot(orders, aes(x = market
   									 , y = sales/1000
   									 , fill = category
   									 )
   			 ) + 
   	geom_bar(stat = "identity"
   					 , position = "dodge" 
   					 , width = 0.7  # And this defines the width of bars themselves
   					 )
   
   # Add data labels
   ggplot(orders, aes(x = market
   									 , y = sales/1000
   									 , fill = category
   									 )
   			 ) + 
   	geom_bar(stat = "identity"
   					 , position = position_dodge(0.7) #  Number inside defines width of "dodging" between groups
   					 , width = 0.7
   					 ) +
   	geom_text(aes(label = sales/1000
   	              , group = category)
   	          ) 
   
   # That's because we need to aggregate data first
   orders.agg <- aggregate(sales ~ market + category
   												, orders
   												, sum)
   # And now it will work
   ggplot(orders.agg, aes(x = market
   											 , y = sales/1000
   											 , fill = category
   											 )
   			 ) + 
   	geom_bar(stat = "identity"
   					 , position = position_dodge(0.7)
   					 , width = 0.7
   					 , color = "black"
   					 ) +
   	geom_text(aes(label = round(sales/1000, 0)) # This defines what to use as lables
   						, position = position_dodge(0.7)  # Shoud be the same number as in geom_bar()
   						, vjust = -0.5                    # This positions labels a little above bars
   						) +
   	theme_bw()
  
#-------------------------------------------------------------------------
#---- Step 6: Pie chart
#-------------------------------------------------------------------------    
  # Very similar to bar chart:
  orders.pie <- aggregate(sales ~ market, orders, sum)
  ggplot(orders.pie, aes(x = ""
   											 , y = sales
   											 , fill = market
   											 )
   			 ) + 
   	geom_bar(width = 1
   	         , stat = "identity"
   	         ) +
    coord_polar(theta = "y" # This defines which variable maps the angle of a circle
                , start = 0 # First item starts at 12 o'clock
                ) + 
   	theme_bw()
 
  # Customize:
  ggplot(orders.pie, aes(x = ""
   											 , y = sales
   											 , fill = market
   											 )
   			 ) + 
   	geom_bar(width = 1
   	         , stat = "identity"
   	         , position = "fill"
   	         , color = "black"
   	         ) +
    scale_fill_brewer(palette = "Accent") + 
    geom_text(aes(label = percent(round(sales / sum(sales), 2)))
              , position = position_fill(vjust = 0.5)
              , size = 6
              , fontface = "bold"
              ) + 
    coord_polar(theta = "y" 
                , start = 0 
                ) +
  	labs(title = "Market Shares"
  	     ) +
   	theme_void() + 
    theme(plot.title = element_text(hjust = 0.5
    																, face = "bold"
    																, size = 20
    																, color = "#912600"
    																)
          , plot.subtitle = element_text(hjust = 0.5
          															 , face = "bold"
          															 , size = 14
          															 , color = "#912600"
          															 )
          , axis.title.x = element_blank()
          , axis.text.x  = element_blank()
          , axis.title.y = element_blank()
          , axis.text.y  = element_blank()
          )
  
#-------------------------------------------------------------------------
#---- Step 7: Calendar Heatmap
#---- See more at http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
#-------------------------------------------------------------------------  
  # Change order.date to type "date"
  orders$order.date <- as.Date(orders$order.date, format = "%Y-%m-%d")
  
  # First we need to aggregate sales by date
  orders.agg <- aggregate(sales ~ order.date
                           , orders
                           , sum
                           )
  
  # Next we need to create a dataset with all the dates from 2012-2015:
  orders.dates <- data.frame(order.date = seq(from = as.Date("2012-01-01")
                                             , to = as.Date("2015-12-31")
                                             , by = "day"
                                             )
                            )
  # Now merge two together:
  orders.daily <- merge(orders.dates, orders.agg
                        , by = "order.date"
                        , all.x = TRUE
                        )
  rm(orders.agg)
  rm(orders.dates)
  # Next we need to create calendar dimensions: days, weeks, months, quaters and years
  # Date formats:
  #   %Y: 4-digit year (1982)
  #   %y: 2-digit year (82)
  #   %m: 2-digit month (01)
  #   %d: 2-digit day of the month (13)
  #   %A: weekday (Wednesday)
  #   %a: abbreviated weekday (Wed)
  #   %B: month (January)
  #   %b: abbreviated month (Jan)
  # Days:
  orders.daily$day <- as.numeric(format(orders.daily$order.date, "%d"))
  orders.daily$weekday <- factor(format(orders.daily$order.date, "%a") # Alternatively, use weekdays() function
                                 , levels = rev(c("Mon"
                                                , "Tue"
                                                , "Wed"
                                                , "Thu"
                                                , "Fri"
                                                , "Sat"
                                                , "Sun"
                                                )
                                              )
                                 , ordered = TRUE
                                 )
  # Week of month, as a difference between week of year current and week of year for 1st day of month
  # Calculate week of the year
  orders.daily$week <- as.numeric(format(orders.daily$order.date, "%W")) + 1 
  # Calculate week of year number for 1st day of every month
  tmp <- as.numeric(format(as.Date(cut(orders.daily$order.date, "month")), "%W"))
  orders.daily$week <- orders.daily$week - tmp
  
  # Months:
  orders.daily$month <- factor(format(orders.daily$order.date, "%b") # Alternatively, use months() function
                               , levels = c("Jan"
                                            , "Feb"
                                            , "Mar"
                                            , "Apr"
                                            , "May"
                                            , "Jun"
                                            , "Jul"
                                            , "Aug"
                                            , "Sep"
                                            , "Oct"
                                            , "Nov"
                                            , "Dec"
                                            )
                               , ordered = TRUE
                               )
  # Quaters:
  orders.daily$quarter <- factor(quarters(orders.daily$order.date)
                                , levels = c("Q1"
                                             , "Q2"
                                             , "Q3"
                                             , "Q4"
                                             )
                                , labels = c("", "", "", "") # To avoid seeing Q1 in pictures
                                , ordered = TRUE
                                )
  
  # Years:
  orders.daily$year <- format(orders.daily$order.date, "%Y")
  
  # Now we can use tiles and facetts to arrange everything
  heatmap1 <- ggplot(orders.daily
                     , aes(x = week
                           , y = weekday
                           , fill = sales
                           )
                     ) +
  	geom_tile(colour = "white") + # This creates a small rectangular for every date 
    facet_grid(year ~ month) +
    scale_fill_gradient(low = "green" # This uses a 2-color gradient scale 
  	                    , high = "dark green"
  	                    , na.value = "gray"
  	                    ) +
    scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6)
                       , labels = c("1", "2", "3", "4", "5", "6")
                       ) +
    labs(x = "Week of Month"
    		 , y = ""
    		 , title = "Daily Total Sales"
    		 ) + 
    theme_bw()
  heatmap1
  
  # Alternatively, we can look at months per quarter per year
  heatmap2 <- ggplot(orders.daily[orders.daily$year == 2015, ]
                     , aes(x = weekday
                           , y = week
                           , fill = sales
                           )
                     ) +
  	geom_tile(colour = "white") + # This creates a small rectangular for every date 
    geom_text(aes(label = day)) + # Day numbers inside tiles
  	scale_fill_gradient(low = "green" # This uses a 2-color gradient scale 
  	                    , high = " dark green"
  	                    , na.value = "gray"
  	                    ) + 
    # facet_rep_wrap is from package "lemon" to keep month labels on every row
    facet_rep_wrap( ~ month   # formula defines which variables identify subsets of data for different facets
                    , ncol = 3 # This is needed to define when to wrap facets
                    , strip.position = "top"
                    , repeat.tick.labels = TRUE
                    ) + 
    scale_y_reverse() + # Proper order of weeks
    scale_x_discrete(limits = rev(levels(orders.daily$weekday))) + # Proper order of weekdays
    labs(x = ""
    		 , y = ""
    		 , title = "Daily Total Sales, 2015"
    		 ) + 
    theme_bw() + 
    theme(strip.background = element_blank() # No background color
  				, strip.placement = "outside"
  				, axis.text.y = element_blank()
  				)
  heatmap2
  
  # Arrange in a grid, similar to base R function par(mfrow = c(1,3))
  grid.arrange(heatmap1, heatmap2, nrow = 1, ncol = 2)
  # See more at:
  # https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
  # http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/

#-------------------------------------------------------------------------
#---- Step 8: Rank Visualizations
#-------------------------------------------------------------------------
  # Create aggregate sales per subcategory per year
  rank.agg <- aggregate(sales ~ sub.category + format(orders$order.date, "%Y")
                           , orders
                           , sum
                           )
  names(rank.agg)[2] <- "year"
  rank.agg$year <- as.numeric(rank.agg$year)
  
  # Ranking based on absolute sales values
  ggplot(rank.agg, aes(x = year, y = sales, color = sub.category)) +
    geom_line() + 
    geom_vline(aes(xintercept = year), linetype="dashed", size = 0.5)
  
  # A few visual adjustments
  ggplot(rank.agg, aes(x = year, y = sales, color = sub.category)) +
    geom_line(linetype = "solid"
              , size = 1.2
              ) + 
    geom_vline(aes(xintercept = year), linetype="dashed", size = 0.5) + 
    scale_x_continuous(breaks = c(2011:2016)
                       , labels = c("", "2012", "2013", "2014", "2015", "")
                       ) + 
    coord_cartesian(xlim = c(2011:2016)) + 
    labs(title = "Sales rankings across subcategories\n"
         , x = "Year"
         , y = "Total sales"
         ) +
    theme_bw()
  
  # Switching to rank positions
  rank(c(2, 1, 11, -2)) # Not what we want
  rev(rank(c(2, 1, 11, -2))) # Not what we want
  rank(-c(2, 1, 11, -2)) # That's what we want
  
  years <- unique(rank.agg$year)
  for (i in 1:length(years)) {
    row.index <- which(rank.agg$year == years[i])
    rank.agg$rank[row.index] <- rank(-rank.agg$sales[row.index]) # Reverse the rankings
  }
  
  # Plot
  ggplot(rank.agg, aes(x = year, y = rank, color = sub.category)) +
    geom_line(linetype = "solid"
              , size = 1.2
              ) + 
    geom_vline(aes(xintercept = year), linetype="dashed", size = 0.5) + 
    scale_x_continuous(breaks = c(2011:2016)
                       , labels = c("", "2012", "2013", "2014", "2015", "")
                       ) + 
    coord_cartesian(xlim = c(2011:2016)) + 
    labs(title = "Sales rankings across subcategories\n"
         , x = "Year"
         , y = "Total sales"
         ) +
    theme_bw()
  
  # Create labels for subcategories and ranks
  ggplot(rank.agg, aes(x = year, y = rank, color = sub.category)) +
    geom_line(linetype = "solid"
              , size = 1.2
              ) + 
    geom_vline(aes(xintercept = year), linetype="dashed", size = 0.5) + 
    geom_text(data = rank.agg[rank.agg$year == min(rank.agg$year), ] # Left side lables
              , aes(label = sub.category
                    , y = rank
                    , x = year - 0.1
                    )
              , color = "black" # To avoid lables being colored same as lines
              , hjust = 1, size = 3.5
              ) +
    geom_text(data = rank.agg[rank.agg$year == max(rank.agg$year), ] # Right side lables
              , aes(label = sub.category
                    , y = rank
                    , x = year + 0.1
                    )
              , color = "black"
              , hjust = 0, size = 3.5
              ) +
    scale_x_continuous(breaks = c(2011:2016)
                       , labels = c("", "2012", "2013", "2014", "2015", "")
                       ) +
    scale_y_reverse(breaks = c(min(rank.agg$rank) : max(rank.agg$rank)) # To reverse ranks so that max rank is on top
                    ) +
    coord_cartesian(xlim = c(2011:2016)) + 
    labs(title = "Sales rankings across subcategories\n"
         , x = "Year"
         , y = "Ranking"
         ) +
    theme_bw() + 
    theme(legend.position = "none")

  