################################################################
#Title: Le Tour - Tidy Tuesday
#Purpose: Make a plot 
#Created by: L Pandori
#Created: 4/7/2020
#Last edited: 4/7/2020
################################################################

##### Data prep and package upload #####
# Clear workspace
rm(lists=ls())

# Load libraries
library(tidyverse) # tidying!
library(lubridate) # dealing w time data
library(png) # Image wrangling
library(grid) # Image wrangling

# Data upload
tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')

# Goal: plot avg speed over time (year of start date) - maybe add in teams to see who is most improved?

  # Steps 
  #   1 - Calculate average speed (distance/time_overall) in km/hr
  #   2 - Get year from start_date (re-format to be %YY)
  #   3 - Plot avg speed over years (add in teams through group?)
  #   4 - Add fun graphics to plot? 
      # (starting line, bicycle, finish line?)

# Step 1 - Calculate average speed

  # select data 
  letour <- select(tdf_winners, c(edition, start_date, winner_team, distance, time_overall, nationality, winner_name))
  
  # calculate avg speed
  letour$speed <- letour$distance/letour$time_overall
  
# Step 2 - Format start_date to year
  letour$year <- year(letour$start_date)
  
# Step 2b - Get distracted, find bike image
  bike = readPNG('bike1.png')
  bike =  rasterGrob(bike, interpolate=TRUE)
  
# Step 3 - Plot avg speed over years
  ggplot(data = letour, mapping = aes(x = year, y = speed, color = speed, size = 4)) +
    # add points and lines (points fill in gaps b/w line segments)
    geom_line(show.legend = FALSE) + 
    geom_point(show.legend = FALSE) +
    # add bike
    annotation_custom(grob=bike,
                      xmin= 1910, xmax= 1975, ymin=30, ymax=43) +
    # nice labels
    labs(title = 'Winning Tour de France Speed Increases over Time') + 
    ylab('Speed (km/hr)') + 
    xlab('Year') + 
    # cleaner theme
    theme_bw(base_size = 18)
  
  # Save plot
  ggsave('LeTour_Plot_04072020.png', width = 10, height = 6)
  
  
    
    
  
  
    
  