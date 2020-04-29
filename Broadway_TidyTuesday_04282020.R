################################################################
#Title: Broadway Musicals - Tidy Tuesday
#Purpose: Make a plot 
#Created by: L Pandori
#Created: 04/28/2020
#Last edited: 04/28/2020
################################################################

##### Package upload #####
# Clear workspace
rm(lists=ls())

# Load libraries
library(tidyverse) # important
library(lubridate) # date/time wrangling
library(cowplot) # plotting
library(patchwork) # multi-panel plots
library(ggThemeAssist) # help format plots
library(RColorBrewer) # nice color palettes
library(hrbrthemes) # nice themes
library(gganimate)
library(ggthemes)

###### Data upload & Tidy ######

# Data upload

broadway <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)

# Tidy

# get most recent decade (weeks ending in 2010 - 2010)
broadway <- broadway %>%
  filter(year(week_ending) >= 2010 & year(week_ending) <= 2019)


# find top 10 grossing shows of decade in this dataset
topten <- broadway %>%
  group_by(show) %>%
  summarize(totalgross = sum(weekly_gross)) %>%
  arrange(desc(totalgross)) %>%
  # just get top 10 
  top_n(n = 10)
  
# continue tidying
broadway <- broadway %>%
  # subset only top 10 obs
  subset(show %in% topten$show) %>%
  # create year column
  mutate(year = year(week_ending)) %>%
  # group data by show and time (week)
  group_by(show, year) %>%
  # calculate totals across theaters
  summarize(year_gross = sum(weekly_gross),
            potential_gross = sum(potential_gross),
            avg_ticket_price = mean(avg_ticket_price),
            top_ticket_price = max(avg_ticket_price),
            seats_sold = sum(seats_sold),
            seats_in_theatre = sum(seats_in_theatre),
            performances = sum(performances),
            previews = sum(previews))

##### Make a plot! #####
# Goal - animate line plot over time of yearly gross in millions of $

# Set theme
lltheme <- theme_set(
  theme_bw() + 
    theme(text = element_text(size = 12),
          # tilt and adjust position of x axis labels
          axis.text.y = element_text(size = 11, color = 'black'),
          axis.text.x = element_text( size = 11, color = 'black'),
          legend.position = 'none'))
  

# Base plot 
 baseplot <- ggplot(data = broadway,
              mapping = aes(x = year, y = (year_gross/1000000),
                               group = show)) + 
        geom_line(mapping = aes(color = show), size = 1.5) +
        geom_text(data = broadway,
                  mapping = aes(x = year, 
                                y = (year_gross/1000000),
                                label = show), hjust = 0) +
        coord_cartesian(xlim = c(2010,2025), ylim = c(0,175)) + 
        xlab('Year') +
        ylab('Box Office Gross (Millions of USD)') + 
        labs(title = 'Earnings of Higest Grossing Musicals of the 2010s',
             subtitle = 'Top 10 in Box Office Gross 2010-2019',
             caption = 'Source: Playbill, The Broadway League') +
        
 # animate
 
 animatedplot <- baseplot + transition_reveal(year) 
 
 animate(animatedplot, end_pause = 20)
        
anim_save('Broadway_TidyTues_Gif.gif')









