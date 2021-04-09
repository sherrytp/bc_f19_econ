##--------------------------------------------------------------------
##----- General
##--------------------------------------------------------------------
  
  # Clear the workspace
  rm(list = ls()) # Clear environment
  gc()            # Clear unused memory
  cat("\f")       # Clear the console
  
  # Prepare needed libraries
  library.list <- c("ggplot2"
                    , "lemon"
                    , "gridExtra" # For Q1
                    , "ggrepel"   # For labels in Q2.b
                    )
  for (i in 1:length(library.list)) {
    if (!library.list[i] %in% rownames(installed.packages())) {
      install.packages(library.list[i], dependencies = TRUE)
    }
    library(library.list[i], character.only = TRUE)
  }
  rm(library.list)
  
  # Set working directory and path to data, if need be
  # setwd("")
  
  # Load data
  sales <- read.csv("../../data/iowa.liquor.r.hw2.sales.csv"
  									, check.names = FALSE
  									, stringsAsFactors = FALSE
  									, na.strings = ""
  									)

  items <- read.csv("../../data/iowa.liquor.r.hw2.items.csv"
  									, check.names = FALSE
  									, stringsAsFactors = FALSE
  									, na.strings = ""
  									)
  # Merge all together
  sales <- merge(sales, items
                 , by = "item.id"
                 , all.x = TRUE
                 , all.y = FALSE
                 )
  rm(items)
  sales$item.id <- NULL
  # Reorder variables
  var.order <- c("date"
                , "category"
                , "subcategory"
                , "item.name"
                , "volume"
                , "price"
                , "sale.bottles"
                , "sale.volume"
                , "sale.dollars"
                )
  sales <- sales[var.order]
  rm(var.order)
  gc()
  
  # Convert variables to proper types
  sales$date <- as.Date(sales$date, format = "%Y-%m-%d")
  sales$category <- factor(sales$category
                           , levels = c("Amaretto"
                                        , "Brandy"
                                        , "Distilled Spirits"
                                        , "Gin"
                                        , "Rum"
                                        , "Schnapps"
                                        , "Tequila"
                                        , "Vodka"
                                        , "Whisky"
                                        , "Misc"
                                        )
                           , ordered = TRUE
                           )

##--------------------------------------------------------------------
##----- Q1
##--------------------------------------------------------------------  
  
  # First we need to aggregate sales by date
  q1.agg <- aggregate(sale.dollars ~ date
                           , sales
                           , sum
                           )
  # Create a dataframe with sale dates in 2015
  q1.dates <- data.frame(date = seq(from = as.Date("2015/01/01")
                                       , to = as.Date("2015/12/31")
                                       , by = "day"
                                       )
                            )
  # Now merge two together
  q1.calendar <- merge(q1.dates, q1.agg, by = "date", all.x = TRUE)
  
  # Then create sales level cut offs to be used in heatmap
  q1.calendar$sale.level <- cut(q1.calendar$sale.dollars
                              , breaks = c(0, 1000000, 1200000, 1400000, 1600000, 2000000)
                              , labels = c("0-1 mln", "1-1.2 mln", "1.2 - 1.4mln", "1.4-1.6 mln", " 1.6-2 mln")
                              , ordered_result = TRUE
                              )
  
  # Next we need to create calendar dimensions: days, weeks, months, quaters and years
  # Days:
  q1.calendar$day <- as.numeric(format(q1.calendar$date, "%d"))
  q1.calendar$weekday <-  factor(weekdays(q1.calendar$date # returns day of the week based on date
                                        , abbreviate = TRUE
                                        )
                               , levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
                               , ordered = TRUE
                               )
  # Week of month:
  q1.calendar$week <- as.numeric(format(q1.calendar$date, "%W")) + 1
  q1.calendar$week <- q1.calendar$week - as.numeric(format(as.Date(cut(q1.calendar$date, "month")), "%W"))
  
  # Months:
  q1.calendar$month <- factor(months(q1.calendar$date)
                               , levels = month.name
                               , labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"
                                            , "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
                                            )
                               , ordered = TRUE
                               )
  # Quaters:
  q1.calendar$quarter <- factor(quarters(q1.calendar$date)
                                , levels = c("Q1"
                                             , "Q2"
                                             , "Q3"
                                             , "Q4"
                                             )
                                , labels = c("", "", "", "") # To avoid seeing quater labels in pictures
                                , ordered = TRUE
                                )
  
  # Years:
  q1.calendar$year <- format(q1.calendar$date, "%Y") # returns year part of date
  
  # 
  ggplot.q1 <- ggplot(q1.calendar, aes(x = weekday
                                       , y = week
                                       , fill = sale.level
                                       )
               ) +
    geom_tile(color = "white" # Lines between tiles, color
  	          , size = 0.5    # Lines between tiles, size
  	          ) + 
    geom_text(aes(label = day)) + # This adds day numbers inside tiles
    scale_y_reverse() + 
    scale_x_discrete(position = "top") + # Have days of week on top of each month
    facet_rep_wrap( ~ quarter + month
                    , ncol = 3
                    , strip.position = "top" # Names of months on top
                    , repeat.tick.labels = TRUE # Have weekday labels under each month
                    ) + 
  	scale_fill_manual(values = c("#D55E00", "#E69F00", "#F0E442", "#74c476", "#1c92d5")
  	                  , na.value = "gray"
  	                  ) +
    labs(x = "", y = "", title = "Daily Total Sales, 2015") +
    guides(fill = guide_legend(nrow = 1)) + # Have all legend items be on one row
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5
                                    , face = "bold"
                                    , size = 20
                                    , color = "#912600"
                                    , margin = margin(t = 10, b = -20) # Push title down, closer to tiles
                                    )
          , plot.subtitle = element_blank()
          , strip.background = element_blank() # No background color
  				, strip.placement = "outside"
  				, strip.text.x = element_text(size = 14, face = "bold", color = "#912600")
  				, panel.grid.major = element_blank()  # Remove grid lines in empty tiles
  				, panel.grid.minor = element_blank()  # Remove grid lines in empty tiles
          , axis.text.y  = element_blank()
  				, axis.text.x  = element_text(size = 10, face = "bold")
  				, axis.ticks = element_blank()
  				, legend.position = "bottom"
  				, legend.direction = "horizontal"
  				, legend.title = element_blank()
  				)
  ggplot.q1
  
  png(file = "q1.png", width = 1920, height = 1920, res = 180)
    ggplot.q1
  dev.off()

##--------------------------------------------------------------------
##----- Q2
##--------------------------------------------------------------------  
  
  # Create a price per volume measure
  sales$price.per.l <- 1000*sales$price/sales$volume
  
  # Box plot, vertical
  ymax <- quantile(sales$price.per.l, 0.95)
  ggplot.q2a <- ggplot(sales[sales$price.per.l < ymax, ]
               , aes(x = category
                     , y = price.per.l
                     , fill = category
                     )
               ) +
    geom_boxplot(outlier.shape = 21 # This shape is one of the few that support fill colors
                 , outlier.size = 2
                 , outlier.color = "black"
                 , outlier.fill = NULL # That's needed for fill to be defined by aes() above
                 ) +
    scale_x_discrete(limits = rev(levels(sales$category))) + # Same order for legend and axis labels 
    scale_y_continuous(breaks = seq(0, round(1.1*ymax, 0), 2) # Custom price axis breaks
                       ) +
    coord_flip() + # Switch x and y axis
    labs(x = "Category"
         , y = "Price per liter, $"
         , title = "Liquor categories, price per liter"
         , subtitle = "Excluding top 5% values"
         ) +
  	theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5
                                , face = "bold"
                                , size = 20
                                , color = "#912600"
                                )
          , plot.subtitle = element_text(hjust = 0.5
                                , face = "bold"
                                , size = 12
                                , color = "#912600"
                                , margin = margin(b = 20)
                                )
        	, panel.grid.minor.y = element_blank()
        	, axis.text.y  = element_text(size = 10, face = "bold")
          , axis.title.y  = element_text(size = 16
                                         , face = "bold"
                                         , color = "#912600"
                                         , margin = margin(r = 10, l = 10)
                                         )
          , axis.text.x  = element_text(size = 10, face = "bold")
          , axis.title.x  = element_text(size = 16
                                         , face = "bold"
                                         , color = "#912600"
                                         , margin = margin(t = 10, b = 10)
                                         )
        	, axis.ticks.y = element_blank()
        	, legend.position = "right"
        	, legend.direction = "vertical"
        	, legend.title = element_blank()
          , legend.text = element_text(size = 10, face = "bold")
        	)
  ggplot.q2a
  
  # Scatter plot
  q2b.agg <- aggregate(cbind(sale.volume)
                            ~ category + subcategory + item.name + price.per.l
                            , sales
                            , sum
                            )
  q2b.agg$price.per.l.w <- q2b.agg$price.per.l*q2b.agg$sale.volume
  
  q2b.agg <- aggregate(cbind(price.per.l.w, sale.volume)
                            ~ category + subcategory
                            , q2b.agg
                            , sum
                            )
  q2b.agg$price.per.l.w <- q2b.agg$price.per.l.w/q2b.agg$sale.volume
  
  # Create labels to be used by ggrepel
  labels <- c("80 Proof Vodkas"
              , "Canadian Whiskies"
              , "Spiced Rum"
              , "Vodkas"
              , "Japanese Whisky"
              , "Miscellaneous Whiskies"
              , "Single Barrel Bourbon Whiskies"
              , "Single Malt Scotch"
              , "Straight Rye Whiskies"
              )
  # Create empty labels variable
  q2b.agg$labels <- NA_character_
  # Make lables be non-empty only for selected subcategories
  q2b.agg$labels[q2b.agg$subcategory %in% labels] <- q2b.agg$subcategory[q2b.agg$subcategory %in% labels]
  
  
  ggplot.q2b <- ggplot(q2b.agg, aes(x = price.per.l.w
                                    , y = sale.volume/1000
                                    , label = labels # Will be empty for most points
                                    )
                       ) +
    geom_point(aes(color = category)
               , fill = "white", shape = 21, stroke = 2, size = 4
               ) +
    geom_text_repel(force = 100
                    , direction = "both"
                    , nudge_x = 5
                    , nudge_y = 5
                    , point.padding	= 1.5
                    , box.padding = 1.5
                    , segment.size = 0.5
                    , size = 4
                    ) +
    scale_x_continuous(breaks = seq(0
                                    , round(max(q2b.agg$price.per.l.w*1.1), 0)
                                    , 5
                                    )
                       ) +
    scale_y_continuous(breaks = seq(0
                                    , round(max(q2b.agg$sale.volume*1.1/1000), 0)
                                    , 250
                                    )
                       ) + 
    labs(x = "Average weighted price per liter, $"
         , y = "Liters sold, thousands"
         , title = "Liquor subcategories"
         , subtitle = "price vs quantity"
         ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5
                                , face = "bold"
                                , size = 20
                                , color = "#912600"
                                , margin = margin(b = 5)
                                )
          , plot.subtitle = element_text(hjust = 0.5
                                , face = "bold"
                                , size = 12
                                , color = "#912600"
                                , margin = margin(b = 20)
                                )
        	, panel.grid.minor.y = element_blank()
        	, axis.text.y  = element_text(size = 10, face = "bold")
          , axis.title.y  = element_text(size = 16
                                         , face = "bold"
                                         , color = "#912600"
                                         , margin = margin(r = 10, l = 10)
                                         )
          , axis.text.x  = element_text(size = 10, face = "bold")
          , axis.title.x  = element_text(size = 16
                                         , face = "bold"
                                         , color = "#912600"
                                         , margin = margin(t = 10, b = 10)
                                         )
        	, legend.position = c(0.9, 0.8)
        	, legend.direction = "vertical"
        	, legend.title = element_blank()
          , legend.text = element_text(size = 10, face = "bold")
          , legend.background = element_rect(fill="transparent")
        	)
  ggplot.q2b
  
  png(file = "q2a.png", width = 2880, height = 1920, res = 180)
    ggplot.q2a
  dev.off()
  
  png(file = "q2b.png", width = 2880, height = 1920, res = 180)
    ggplot.q2b
  dev.off()
  
  png(file = "q2c.png", width = 2880, height = 1920, res = 180)
    grid.arrange(ggplot.q2a, ggplot.q2b, nrow = 1, ncol = 2)
  dev.off()

##--------------------------------------------------------------------
##----- Q3 - sales dynamics by category
##--------------------------------------------------------------------
  
  q3a.agg <- aggregate(cbind(sale.dollars) ~ category + months(date)
                       , sales
                       , sum
                       )
  q3a.agg$month <- factor(q3a.agg$`months(date)`
                               , levels = month.name
                               , labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"
                                            , "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
                                            )
                               , ordered = TRUE
                               )
  q3a.agg$`months(date)` <- NULL
  q3a.agg <- merge(q3a.agg, aggregate(cbind(sale.dollars) ~ category
                                    , q3a.agg, sum
                                    )
                  , by = c("category")
                  , all = TRUE
                  )
  names(q3a.agg) <- c("category", "sales.monthly", "month", "sales.annual")
  q3a.agg$sales.monthly.share <- round(100*q3a.agg$sales.monthly/q3a.agg$sales.annual, 2)
  
  ggplot.q3a <- ggplot(q3a.agg, aes(x = month
                           , y = sales.monthly.share
                           , group = category
                           , color = category
                           )
               ) +
    geom_line(size = 1.5) +
    scale_y_continuous(breaks = seq(0, round(1.1*max(q3a.agg$sales.monthly.share), 0), 2)
                       #, limits = c(0, round(1.2*max(q3a.agg$sales.monthly.share), 0))
                       ) +
    labs(x = "Month"
         , y = "Share of total sales"
         , title = "Share of total sales per month, %"
         ) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5
                                , face = "bold"
                                , size = 20
                                , color = "#912600"
                                , margin = margin(b = 10)
                                )
        	, axis.text.y  = element_text(size = 10, face = "bold")
          , axis.title.y  = element_blank()
          , axis.text.x  = element_text(size = 10, face = "bold")
          , axis.title.x  = element_text(size = 16
                                         , face = "bold"
                                         , color = "#912600"
                                         , margin = margin(t = 10, b = 10)
                                         )
        	, legend.position = "right"
        	, legend.direction = "vertical"
        	, legend.title = element_blank()
          , legend.text = element_text(size = 10, face = "bold")
        	)
  ggplot.q3a
  
  q3b.agg <- aggregate(cbind(sale.dollars) ~ category + weekdays(date
                                                                 , abbreviate = TRUE)
                       , sales
                       , sum
                       )
  q3b.agg$weekday <- factor(q3b.agg[, 2]
                               , levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
                               , ordered = TRUE
                               )
  q3b.agg[, 2] <- NULL
  q3b.agg <- merge(q3b.agg, aggregate(cbind(sale.dollars) ~ category
                                    , q3b.agg, sum
                                    )
                  , by = c("category")
                  , all = TRUE
                  )
  names(q3b.agg) <- c("category", "sales.weekday", "weekday", "sales.annual")
  q3b.agg$sales.weekday.share <- round(100*q3b.agg$sales.weekday/q3b.agg$sales.annual, 2)
  
  ggplot.q3b <- ggplot(q3b.agg, aes(x = weekday
                           , y = sales.weekday.share
                           , group = category
                           , color = category
                           )
               ) +
    geom_line(size = 1.5) +
    labs(x = "Weekday"
         , y = "Share of sales"
         , title = "Share of total sales per weekday, %"
         ) +
    scale_y_continuous(breaks = seq(0, round(1.1*max(q3b.agg$sales.weekday.share), 0), 5)
                       #, limits = c(0, round(1.2*max(q3b.agg$sales.weekday.share), 0))
                       ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5
                                , face = "bold"
                                , size = 20
                                , color = "#912600"
                                , margin = margin(b = 10)
                                )
        	, axis.text.y  = element_text(size = 10, face = "bold")
          , axis.title.y  = element_blank()
          , axis.text.x  = element_text(size = 10, face = "bold")
          , axis.title.x  = element_text(size = 16
                                         , face = "bold"
                                         , color = "#912600"
                                         , margin = margin(t = 10, b = 10)
                                         )
        	, legend.position = "none"
        	)

  ggplot.q3b
  
  ggplot.q3c <- grid.arrange(ggplot.q3a, ggplot.q3b
                            , nrow = 1, ncol = 2
                            , widths = c(2, 1)
                            )
  ggplot.q3c
  
  png(file = "q3a.png", width = 2880, height = 1920, res = 180)
    ggplot.q3a
  dev.off()
  
  png(file = "q3b.png", width = 2880, height = 1920, res = 180)
    ggplot.q3b
  dev.off()
  
  png(file = "q3c.png", width = 2880, height = 1920, res = 180)
    grid.arrange(ggplot.q3a, ggplot.q3b
                            , nrow = 1, ncol = 2
                            , widths = c(2, 1)
                            )
  dev.off()

##--------------------------------------------------------------------
##----- Q4 -  Ranking of categories per sales/volume
##--------------------------------------------------------------------
  # Aggregate category sales by dollars and liters
  q4.agg <- aggregate(cbind(sale.dollars, sale.volume, sale.bottles) ~ category
                           , sales
                           , sum
                           )
  # Add percentage of total
  q4.agg$share.dollars <- round(100*q4.agg$sale.dollars/sum(q4.agg$sale.dollars), 2)
  q4.agg$share.volume <- round(100*q4.agg$sale.volume/sum(q4.agg$sale.volume), 2)
  q4.agg$share.bottles <- round(100*q4.agg$sale.bottles/sum(q4.agg$sale.bottles), 2)
  
  # Calculate ranks
  q4.agg$rank.dollars <- rank(-q4.agg$share.dollars)
  q4.agg$rank.volume <- rank(-q4.agg$share.volume)
  q4.agg$rank.bottles <- rank(-q4.agg$share.bottles)
  
  # Create artficial x-coordinates to be used as horizontal spacing between rank hierarchies
  q4.agg$x1 <- 1
  q4.agg$x2 <- 2
  q4.agg$x3 <- 3
  
  ggplot.q4 <- ggplot(q4.agg) + # No aes() here because different geometries below use different aesthetics
    geom_segment(aes(x = x1
                     , xend = x2
                     , y = rank.dollars
                     , yend = rank.volume
                     , color = category
                     )
                 , size = 1.25, show.legend = F
                 ) + 
    geom_segment(aes(x = x2
                     , xend = x3
                     , y = rank.volume
                     , yend = rank.bottles
                     , color = category
                     )
                 , size = 1.25, show.legend = F
                 ) +
    geom_vline(xintercept = 1, linetype="dashed", size = .5) + 
    geom_vline(xintercept = 2, linetype="dashed", size = .5) +
    geom_vline(xintercept = 3, linetype="dashed", size = .5) +
    geom_text(aes(label = category
                  , y = rank.dollars
                  , x = x1 - 0.1 # Slight to the left of first vertical line
                  )
              , hjust = 1, size = 4.5, fontface = "bold"
              ) + 
    geom_text(aes(label = category
                  , y = rank.bottles
                  , x = x3 + 0.1 # Slightly to the right of third vertical line
                  )
              , hjust = 0, size = 4.5, fontface = "bold"
              ) +
    # Define breaks/ticks on y axis
    scale_y_reverse(breaks = c(min(q4.agg$rank.dollars) : max(q4.agg$rank.dollars))
                       ) + 
    # Define breaks/ticks on x axis
    scale_x_continuous(breaks = c(0, 1, 2, 3, 4) # It start at 0 and goes to 4 to create empty spacing on both sides
                       , labels = c("", "Sales, $", "Sales, liters", "Sales, bottles", "") # Empty lables for 0 and 4
                       , sec.axis = dup_axis() # This duplicates x axis above graph
                       ) + 
    coord_cartesian(xlim = c(0, 1, 2, 3, 4)) + # This forces x axis to use limits we specified above
    labs(title = "Liquor Category Rankings"
         ) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5
                                , face = "bold"
                                , size = 20
                                , color = "#912600"
                                , margin = margin(b = 30)
                                )
        	, axis.text.y  = element_blank()
          , axis.title.y  = element_blank()
          , axis.ticks.y = element_blank()
          , axis.text.x  = element_text(size = 12
                                         , face = "bold"
                                         , color = "#912600"
                                         , margin = margin(t = 10, b = 10)
                                         )
          , axis.title.x  = element_blank()
          , axis.ticks.x = element_blank()
          , panel.border = element_blank()
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
        	, legend.position = "none"
          )
  ggplot.q4
  png(file = "q4.png", width = 2880, height = 1920, res = 180)
    ggplot.q4
  dev.off()
  
  