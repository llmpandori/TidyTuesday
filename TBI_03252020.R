################################################################
#Title: TBI - Tidy Tuesday
#Purpose: Make a plot 
#Created by: L Pandori
#Created: 3/24/2020
#Last edited: 03/25/2020
################################################################

##### Data prep and package upload #####
#Clear workspace
rm(lists=ls())

#load libraries
#library(RColorBrewer) # make color palettes
#library(zoo)      # convert date/time formats (lubridate)
library(tidyverse)

# download data
tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')

# Learning section --------------- 
# shorter names for mechanisms (recode in dplyr ~ revalue in plyr)
tbi_age$injury2 <- recode(tbi_age$injury_mechanism, 'Motor Vehicle Crashes' = 'Vehicle Crash', 'Unintentional Falls' = 'Fall', 'Unintentionally struck by or against an object' = 'Object Strike', 'Other unintentional injury, mechanism unspecified' = 'Other - Unintent.', 'Intentional self-harm' = 'Self-Harm', 'Assault' = 'Assault', 'Other or no mechanism specified' = "Other - No Mech.")

# filter relevant data
tbi <- tbi_age %>% 
  filter(type == 'Emergency Department Visit') %>% # only ER Visits
  filter(age_group != '0-17') %>% # remove strange age class
  filter(age_group != 'Total') # remove totals
# create plot 
ggplot(tbi) + # specify dataset 
  geom_col(aes(x = injury2, y = number_est)) + # bar
  geom_jitter(aes(x = injury2, y = number_est, color = age_group)) + # points
  ggtitle('Brain Injury ER Visits by Mechanism and Age') + # title
  xlab('Injury Type') + # x axis label
  ylab ('Number') + # y axis label
  theme_bw() # clean up theme

ggsave('TBI_plot_03242020.png', width = 8, height = 4)


  
