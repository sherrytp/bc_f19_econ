##--------------------------------------------------------------------
##----- ADEC 7910: Software Tools for Data Analysis
##----- R Homework 2 Template -----------------------
##------Name: Peng Tian -----------------------------
##------Date: April 15, 2019 ------------------------

ggplot(df, aes(x = , y = , color = , shape = ) 		 # similar to tableau filter 
geom_point(color = , fill = , size = ) 			# create scatter plots
geom_smooth(color = , weight = # line thickness) 	trend line with scatter plots 
labs(title = “”, subtitle = “”, x = “”, y = “”) 		# all titles and labels 
theme_bw() 						# built-in themes? 
  theme()					 # overwrite steps of theme_bw()
  theme(plot.title = element_text(hjust = 0.5          # Title alignment, 0 - left, 1 - right
      , face = "bold"      # font face - bold, italic, etc.
      , size = 20          # font size
      , color = "#912600") # font color
        , plot.subtitle = element_text(hjust = 0.5     # Horizontal alignment, 0.5 = center
      , face = "bold" # Bold font
      , size = 14     # Font size
      , color = "#912600"  # Font color
      , margin = margin(b = 10)
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
      , legend.position = c(0.1, 0.2)    # Or use "bottom", "top", "left", "right"
      , legend.key.size = unit(1, 'lines') # Size of elements inside legend box
      , legend.title = element_blank()    # 			VERY IMPORT Hides legend title
      , legend.background = element_rect(size = 0.5)
      , linetype = "solid"
      , color = "black") 

geom_histogram(bins = , bin width = , aes(y = ..density..) 			# create histogram 
scale_x_continuous(breaks = seq(from = 0, to = n, by = n, labels = seq(from, to, by) / percent) 
geom_vline(aes(xintercept = mean() or median(), color = , linetype = “dashed”, size = )	 # create vertical line 
annotate()									# under histogram?? 
geom_density(aes(y = ..density..), color = , size = ) 				# add density curve 
facet_wrap( ~ category, scale = “fixed” or “free”, ncol = 1, now = 3) 	 # facet histograms 
theme(strip.text.x = element_text(size = 12
      , face = "bold")
      , strip.background = element_blank() # No background color
      , strip.placement = "outside"
      , legend.position = "none"  
              )
geom_boxplot(alpha = transparancy, color = , size = )			# create box-plot 
coord_flip() 					# VERY IMPORT, flip axis around, can use anyway
factor(df, levels = c(“”, “”, “”, ordered = TRUE) 				# rank chars in order 
geom_bar(stat = “identity”, position = “stack”) 				# create stacked bars 
geom_bar(stat = “identity”, position = “dodge”) 				# non stacked bar chart
  .pie <- aggregate(a ~ b, df, sum) 
ggplot(.pie, aes(x = “”, y = a, fill = b) + 
geom_bar(width = 1, stat = “identity”) + 
coord_polar(theta = “y”, stat = 0) 					# create pie charts 


##--------------------------------------------------------------------
##----- General
##--------------------------------------------------------------------
  
  # Clear the workspace
  rm(list = ls()) # Clear environment
  gc()            # Clear unused memory
  cat("\f")       # Clear the console
  
  # Prepare needed libraries
  library.list <- c("readxl"
                    , "ggplot2"
                    , "lemon"
                    , "gridExtra"
                    , "ggrepel"
                    )
  for (i in 1:length(library.list)) {
    if (!library.list[i] %in% rownames(installed.packages())) {
      install.packages(library.list[i], dependencies = TRUE)
    }
    library(library.list[i], character.only = TRUE)
  }
  rm(library.list, i)
  
  # Set working directory and path to data, if need be
  # setwd("")
  
  # Load data
  sales <- read.csv(file.choose()
  									, check.names = FALSE
  									, stringsAsFactors = FALSE
  									, na.strings = ""
  									)
  items <- read.csv(file.choose()
  									, check.names = FALSE
  									, stringsAsFactors = FALSE
  									, na.strings = ""
  									)
  
  # Merge data together
  sales <- merge(sales, items[,c("item.id", "item.name", "volume", "category", "subcategory")]
                 , by = "item.id"
                 , all = FALSE) 
  
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
  # Remove unnecessary variables and objects
  rm(items, var.order)

##--------------------------------------------------------------------
##----- Q1
##--------------------------------------------------------------------  
  
  # Q1 chart
  sales$date <- as.Date(sales$date, format = "%Y-%m-%d")
  q1.agg <- aggregate(sale.dollars ~ date
                      , sales
                      , sum
  )
  q1.dates <- data.frame(date = seq(from = as.Date("2015/01/01")
                                    , to = as.Date("2015/12/31")
                                    , by = "day"
  ))
  q1.calendar <- merge(q1.dates, q1.agg, by = "date", all.x = TRUE)
  q1.calendar$sales <- cut(q1.calendar$sale.dollars
                                , breaks = c(0, 1000000, 1200000, 1400000, 1600000, 2000000)
                                , labels = c("0-1 mln", "1-1.2 mln", "1.2 - 1.4mln", "1.4-1.6 mln", " 1.6-2 mln")
                                , ordered_result = TRUE
  )

  # Next we need to create calendar dimensions: days, weeks, months, quaters and years
  # Days: 
  q1.calendar$day <- as.numeric(format(q1.calendar$date, "%d"))
  q1.calendar$weekday <- factor(format(q1.calendar$date, "%a", abbreviate = TRUE)
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
  q1.calendar$week <- as.numeric(format(q1.calendar$date, "%W")) + 1 
  # Calculate week of year number for 1st day of every month
  tmp <- as.numeric(format(as.Date(cut(q1.calendar$date, "month")), "%W"))
  q1.calendar$week <- q1.calendar$week - tmp
  
  # Months: 
  q1.calendar$month <- factor(format(q1.calendar$date, "%b") 
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
  q1.calendar$quarter <- factor(quarters(q1.calendar$date)
                                 , levels = c("Q1"
                                              , "Q2"
                                              , "Q3"
                                              , "Q4"
                                 )
                                 , labels = c("", "", "", "") # To avoid seeing Q1 in pictures
                                 , ordered = TRUE
  )
  # Years:
  q1.calendar$year <- format(q1.calendar$date, "%Y")
  # ggplot 
  ggplot.q1 <- ggplot(q1.calendar, aes(x = weekday
                                       , y = week
                                       , fill = sales)
                      ) +
    geom_tile(color = "white" 
              , size = 0.5
              ) + 
    geom_text(aes(label = day)) + 
    scale_y_reverse() + # Proper order of weeks
    scale_x_discrete(limits = rev(levels(q1.calendar$weekday))) + # Proper order of weekdays
    facet_rep_wrap( ~ quarter + month
                    , ncol = 3
                    , strip.position = "top" 
                    , repeat.tick.labels = TRUE
                    ) + 
    scale_fill_manual(values = c("#D55E00", "#E69F00", "#F0E442", "#74c476", "#1c92d5")
                      , na.value = "gray"
                      ) +
    labs(x = "", y = "", title = "Daily Total Sales, 2015") +
    guides(fill = guide_legend(nrow = 1)) + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5
                                    , face = "bold"
                                    , size = 20
                                    , color = "#912600"
                                    , margin = margin(t = 10, b = -20)
                                    ), plot.subtitle = element_blank()
          , legend.position = "bottom")
  ggplot.q1
  
  # Export Q1 chart
  png(file = "q1.png", width = 1920, height = 1920, res = 180)
    ggplot.q1
  dev.off()

##--------------------------------------------------------------------
##----- Q2
##--------------------------------------------------------------------  
  
  # Q2a Box plot, vertical
  # Create a price per volume measure 
  sales$price.per.l <- 1000*sales$price/sales$volume
  # Create the 95% bottom cutoff 
  ymax <- quantile(sales$price.per.l, 0.95)
  
  ggplot.q2a <- ggplot(sales[sales$price.per.l < ymax, ]
                       , aes(x = category
                             , y = price.per.l
                             , fill = category
                             )
  ) + 
    geom_boxplot(outlier.shape = 21, outlier.size = 3) + 
    scale_x_discrete(limits = rev(levels(sales$category))) + # Same order for legend and axis labels 
    scale_y_continuous(breaks = seq(0, round(1.1*ymax, 0), 2)) + 
    coord_flip() + # Switch x and y axis 
    labs(x = "Category", y = "price per liter in dollars", 
         title = "Liquor categories, price per liter", 
         subtitle = "Excluding 5% top") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5
                                    , face = "bold"
                                    , size = 20
                                    , color = "#912600" )
                                    , legend.title = element_blank()    # make category title disappear 
    , plot.subtitle = element_text(hjust = 0.5
                                   , face = "bold"
                                   , size = 12
                                   , color = "#912600"
                                   , margin = margin(b = 10)
    ))
  ggplot.q2a
  
  # Q2b chart
  # Create aggregate measures for total volumn sold and average weighted price per liter 
  q2b.agg <- aggregate(cbind(sale.volume)
                       ~ category + subcategory + item.name + sale.volume + price.per.l
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
  # Create labels for subcategories
  labels <- c("80 Proof Vodkas"
              , "Canadian Whiskies"
              , "Spiced Rum"
              , "Vodkas"
              , "Miscellaneous Whiskies"
              , "Straight Rye Whiskies"
              , "Single Malt Scotch"
              , "Single Barrel Bourbon Whiskies"
              , "Japanese Whisky")
  
  # Create empty labels variable
  q2b.agg$labels <- NA_character_
  # Make lables be non-empty only for selected subcategories
  q2b.agg$labels[q2b.agg$subcategory %in% labels] <- q2b.agg$subcategory[q2b.agg$subcategory %in% labels]
  
  ggplot.q2b <- ggplot(q2b.agg, aes(x = price.per.l.w
                                    , y = sale.volume/1000
                                    , label = labels 
                                    )) + 
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
    labs(x = "Average price per liter, $"
             , y = "Liters sold, thousands"
             , title = "Liquor subcategories"
             , subtitle = "price vs quantity"
         ) + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5
                                          , face = "bold"
                                          , size = 20
                                          , color = "#912600")
                , plot.subtitle = element_text(hjust = 0.5
                                               , face = "bold"
                                               , size = 14
                                               , color = "#912600"
                                               , margin = margin(b = 10))
          ,legend.title = element_blank())    # make category title disappear 
  ggplot.q2b
  
  # Export Q2a chart
  png(file = "q2a.png", width = 2880, height = 1920, res = 180)
    ggplot.q2a
  dev.off()
  
  # Export Q2b chart
  png(file = "q2b.png", width = 2880, height = 1920, res = 180)
    ggplot.q2b
  dev.off()
  
  # Export Q2c chart
  png(file = "q2c.png", width = 2880, height = 1920, res = 180)
    grid.arrange(ggplot.q2a, ggplot.q2b
                 , nrow = 1
                 , ncol = 2
                 )
  dev.off()
  
##--------------------------------------------------------------------
##----- Q3
##--------------------------------------------------------------------
  
  # Q3a chart
  q3a.agg <- aggregate(cbind(sale.dollars) ~ category + months(date)
                       , sales
                       , sum
  )
  q3a.agg$month <- factor(q3a.agg$`months(date)`
                          , levels = month.name
                          , labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"
                                       , "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
                          )
                          , ordered = TRUE)
  q3a.agg$`months(date)` <- NULL     # elimate the NULL possibilities 
  q3a.agg <- merge(q3a.agg, aggregate(cbind(sale.dollars) ~ category
                                      , q3a.agg, sum
  )
  , by = c("category")
  , all = TRUE)
  colnames(q3a.agg)
  colnames(q3a.agg)[colnames(q3a.agg) == "sale.dollars.x"] <- "sales.monthly"
  colnames(q3a.agg)[colnames(q3a.agg) == "sale.dollars.y"] <- "sales.annual" 
  q3a.agg$sales.monthly.share <- q3a.agg$sales.monthly/q3a.agg$sales.annual *100

  ggplot.q3a <- ggplot(q3a.agg, aes(x = month
                                    , y = sales.monthly.share
                                    , group = category
                                    , color = category)
                       ) + geom_line(size = 1.5) +
    scale_y_continuous(breaks = seq(0, round(1.1*max(q3a.agg$sales.monthly.share), 0), 2)
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
                                    , margin = margin(b = 10))
    , legend.title = element_blank()    # make category title disappear 
    , axis.text.x  = element_text(size = 10)
    , axis.title.x  = element_text(size = 12
                                   , face = "bold"
                                   , color = "#912600"
                                   , margin = margin(t = 10, b = 10))
    , axis.text.y  = element_text(size = 10)
    , axis.title.y  = element_blank())
  ggplot.q3a
  
  # Q3b chart 
  q3b.agg <- aggregate(cbind(sale.dollars) ~ category + weekdays(date, abbreviate = TRUE)
                       , sales
                       , sum)
  q3b.agg$weekday <- factor(q3b.agg[, 2]
                            , levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
                            , ordered = TRUE)
  q3b.agg[, 2] <- NULL
  q3b.agg <- merge(q3b.agg, aggregate(cbind(sale.dollars) ~ category
                                      , q3b.agg, sum)
                   , by = c("category")
                   , all = TRUE)
  colnames(q3b.agg)[colnames(q3b.agg) == "sale.dollars.x"] <- "sales.weekday"
  colnames(q3b.agg)[colnames(q3b.agg) == "sale.dollars.y"] <- "sales.annual"
  q3b.agg$sales.weekday.share <- q3b.agg$sales.weekday/q3b.agg$sales.annual * 100
  
  ggplot.q3b <- ggplot(q3b.agg, aes(x = weekday
                                    , y = sales.weekday.share
                                    , group = category
                                    , color = category)
                       ) +
    geom_line(size = 1.5) +
    labs(x = "Weekday"
         , y = "Share of sales"
         , title = "Share of total sales per weekday, %"
         ) +
    scale_y_continuous(breaks = seq(0, round(1.1*max(q3b.agg$sales.weekday.share), 0), 5)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5
                                  , face = "bold"
                                  , size = 20
                                  , color = "#912600"
                                  , margin = margin(b = 10))
        , legend.title = element_blank()    # make category title disappear 
        , axis.text.x  = element_text(size = 10)
        , axis.title.x  = element_text(size = 12
                                       , face = "bold"
                                       , color = "#912600"
                                       , margin = margin(t = 10, b = 10))
        , axis.text.y  = element_text(size = 10)
        , axis.title.y  = element_blank())
 
  ggplot.q3b
  
  # Export Q3a chart
  png(file = "q3a.png", width = 2880, height = 1920, res = 180)
    ggplot.q2a
  dev.off()
  
  # Export Q3b chart
  png(file = "q3b.png", width = 2880, height = 1920, res = 180)
    ggplot.q2b
  dev.off()
  
  # Export Q3c chart
  ggplot.q3c <- grid.arrange(ggplot.q3a, ggplot.q3b
                             , nrow = 1, ncol = 2
                             , widths = c(2, 1))
  png(file = "q3c.png", width = 2880, height = 1920, res = 180)
    grid.arrange(ggplot.q3a, ggplot.q3b
                            , nrow = 1, ncol = 2
                            , widths = c(2, 1)
                            )
  dev.off()
  
##--------------------------------------------------------------------
##----- Q4
##--------------------------------------------------------------------
  
  # Q4 chart 
  q4.agg <- aggregate(cbind(sale.dollars, sale.volume, sale.bottles) ~ category
                      , sales, sum)
  q4.agg$share.dollars <- round(100*q4.agg$sale.dollars/sum(q4.agg$sale.dollars), 2)
  q4.agg$share.volume <- round(100*q4.agg$sale.volume/sum(q4.agg$sale.volume), 2)
  q4.agg$share.bottles <- round(100*q4.agg$sale.bottles/sum(q4.agg$sale.bottles), 2)
  q4.agg$rank.dollars <- rank(-q4.agg$share.dollars)
  q4.agg$rank.volume <- rank(-q4.agg$share.volume)
  q4.agg$rank.bottles <- rank(-q4.agg$share.bottles)
  
  ggplot.q4 <- ggplot(q4.agg
                      ) + geom_segment(aes(x = rep(1, nrow(q4.agg))
                     , xend = rep(2, nrow(q4.agg))
                     , y = rank.dollars
                     , yend = rank.volume
                     , color = category)
                 , size = 1.25, show.legend = F 
                 ) + 
    geom_segment(aes(x = rep(2, nrow(q4.agg))
                     , xend = rep(3, nrow(q4.agg))
                     , y = rank.volume
                     , yend = rank.bottles
                     , color = category)
                 , size = 1.25, show.legend = F
                 ) +
    geom_vline(xintercept = 1, linetype="dashed", size = .5) + 
    geom_vline(xintercept = 2, linetype="dashed", size = .5) +
    geom_vline(xintercept = 3, linetype="dashed", size = .5) +
    geom_text(aes(label = q4.agg$category
                  , y = q4.agg$rank.dollars
                  , x = rep(1, nrow(q4.agg)) - 0.1)
              , hjust = 1, size = 4.5, fontface = "bold"
              ) + 
    geom_text(aes(label = q4.agg$category
                  , y = q4.agg$rank.bottles
                  , x = rep(3, nrow(q4.agg)) + 0.1)
              , hjust = 0, size = 4.5, fontface = "bold"
              ) +
    scale_y_reverse(breaks = c(min(q4.agg$rank.dollars) : max(q4.agg$rank.dollars))
                    ) +
    scale_x_continuous(breaks = c(0, 1, 2, 3, 4)
                       , labels = c("", "Sales, $", "Sales, liters", "Sales, bottles", "")
                       ) + labs(title = "Liquor Category Rankings") +
    coord_cartesian(xlim = c(0, 1, 2, 3, 4)) + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5
                                  , face = "bold"
                                  , size = 20
                                  , color = "#912600"
                                  , margin = margin(b = 10))
        , axis.text.x  = element_text(size = 10)
        , axis.title.x  = element_text(size = 12
                                       , face = "bold"
                                       , color = "#912600"
                                       , margin = margin(t = 10, b = 10))
        , legend.position = "none"
        , axis.text.y  = element_text(size = 10)
        , axis.title.y  = element_blank())
ggplot.q4

  # Remove aggregate and unused variables 
rm(q1.agg, q1.calendar, q1.dates, tmp)
rm(q2b.agg, labels, ymax)
rm(q3a.agg, q3b.agg)
rm(q4.agg)
  
  # Export Q4 chart
  png(file = "q4.png", width = 1920, height = 1920, res = 180)
    ggplot.q4
  dev.off()
