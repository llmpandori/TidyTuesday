################################################################
#Title: Beer - Tidy Tuesday
#Purpose: Make a map with color-coding
#Created by: L Pandori
#Created: 3/31/2020
################################################################
# Inspiration: https://www.r-graph-gallery.com/327-chloropleth-map-from-geojson-with-ggplot2.html

##### Data prep and package upload #####
#Clear workspace
rm(lists=ls())

# Load libraries
library(tidyverse) # load tidyverse
library(usmap) # map of USA

# Load data
  # beer production by state
  beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

  # data from US census bureau
  uspop <- read_csv("US_Population_2008_2018.csv")
  
# Steps: 
  # Step 1 - get map file
  # Step 2 - prep data by merging population & beer production
  # Step 3 - calculate metric (beer/pop)
  # Step 4 - put data on map

# Step 1 - get map file
  map <- plot_usmap(regions = 'state')
  map2 <- tidy(map)
# Step 2 - prep data and merge
  # prep beer data
  beer <- beer_states %>% 
    filter(year %in% c('2018')) %>% # only 2018
    filter(type == 'Bottles and Cans') # limit to bottles & cans data
  
  # merge state and beer data
  beerpop <- merge(uspop, beer, by = c('state'))
    # get rid of extra column
    beerpop$X5 <- NULL

# Step 3 - calculate metric (barrels/ 1,000 people for 2008, 2018)
  beerpop$percap <- (beerpop$barrels/beerpop$pop)*1000

# Step 4 - merge map and population data
  # limit beerpopdata
  beerpop2 <- beerpop %>% select(state, percap)
  beerpop2 <- rename(beerpop2, abbr = state)
  
  # merge w existing map data in package (hint from Danielle Becker's GitHub - Thank you!)
  data('statepop')
  statepop2 <- merge(beerpop2, statepop, by = 'abbr')
    
  # add to map
  plot_usmap(data = statepop2, values = 'percap') + 
    scale_fill_continuous('Barrels per 1000 people') + 
    theme(legend.position = "top") + 
    ggtitle('Bottle & Can Beer Production in 2018') # title

# save plot
  ggsave('BeerProduction_Plot_03302020.png', width = 4, height = 4)
  