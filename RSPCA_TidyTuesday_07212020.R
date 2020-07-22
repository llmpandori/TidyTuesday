################################################################
# Title: RSPCA
# Purpose: make a cool table...and a graph 
# Created by: L Pandori
# Created: 07/14/2020
# Last edited: 07/14/2020
################################################################

##### package upload #####
library(tidyverse) # it is tuesday
library(formattable) # repeat performance? of nice data
library(tidytuesdayR) # get tidy tuesday data
library(Hmisc)

##### data upload #####
animal_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv')

##### plan of action #####
# goal: table & plot summarizing animal complaint data
  # plot - type of complaint over time
  # table - percent of each type of complaint for dogs vs cats

# steps: 
  # 1 - tidy data for plot & table
  # 2 - make plot and table (w/ formatting + images)
  # 3 - join with patchwork?

##### tidy data #####

# create date column using month and year info
complaints <- animal_complaints %>%
  separate(`Date Received`, into = c('month', 'year'), sep = ' ') %>%
  mutate(month = substr(month, 1,3),
         month_numeric = match(month,month.abb),
        `Month and Year` = 
    lubridate::ymd(paste(year, month, '1')),
    `Animal Type` = Hmisc::capitalize(`Animal Type`)) %>%
  select(`Animal Type`, `Complaint Type`, `Month and Year`)


# create summary data for plot 
tabledata <- complaints %>%
  group_by(`Complaint Type`, `Animal Type`) %>% tally() %>%
# calculate % of each complaint type
  mutate(Percent = round((n/nrow(complaints))*100, digits = 2)) %>%
  select(-c(n))


# make categories for cats and dogs
tabledata <- pivot_wider(tabledata, names_from = `Animal Type`, values_from = Percent, values_fill = 0)


plotdata <- complaints %>%
  group_by(`Complaint Type`, `Animal Type`, `Month and Year`) %>% tally()


# custom colors from :https://www.littlemissdata.com/blog/prettytables
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

# create plot of complaints over time
catdog <- ggplot(data = plotdata, mapping = aes(x = `Month and Year`, y = n, group = `Animal Type`)) +
  geom_smooth(mapping = aes(color = `Animal Type`)) +
  scale_color_manual(values = c(customGreen, customRed)) +
  ylab('Number of Complaints') +
  ggtitle('Dogs Account for More RSPCA Complaints than Cats') +
  theme_bw()

ggsave('catdog.png', width = 6, height = 4)

# create table with summary data for complaint types
formattable(tabledata,
            align = c('l','c','c'),
            list(
            `Dog` = color_tile(customRed),
            `Cat` = color_tile(customGreen)))
              
            

