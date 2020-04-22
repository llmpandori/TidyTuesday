################################################################
#Title: GDPR Violations
#Purpose: Make a plot 
#Created by: L Pandori
#Created: 4/21/2020
#Last edited: 4/21/2020
################################################################

##### Data prep and package upload #####
# Clear workspace
rm(lists=ls())

# Load libraries
library(tidyverse) 
library(ggpubr)
library(cowplot)
library(patchwork)
library(gganimate)
library(lubridate)

# Data upload
gdpr <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')

# Goal: Stacked bar plot with fines by country, animate across years
  # Step 1 - Isolate data of interest (country, fine, year)
  # Step 2 - Create base plot
  # Step 3 - Animate

# Step 1 -----
# format date column
gdpr$date <- as.Date(gdpr$date , format = "%m/%d/%Y")
gdpr$year <- year(gdpr$date)

# tidy 
gdpr <- gdpr %>% 
  # get columns of interest
  select(name, price, year) %>%
  # isolate years of interest (very few in 1970, then only 2018-2019)
  filter(year != 1970) %>%
  arrange(year) %>%
  # make summary table of fines by country & year
  group_by(name, year) %>%
  summarize(totalprice = sum(price),
            numfines = length(name),
            avgfine = mean(price),
            sdfine = sd(price)) %>%
  mutate(milprice = totalprice/1000000)

  # make df with total # of fines per country for bar labels
  gdpr2 <- gdpr %>%
    group_by(name) %>%
    summarize(milprice2 = sum(milprice),
              numfines2 = sum(numfines))
  
  gdpr$year <- as.factor(gdpr$year)

# Step 2 Plot(s?)! -----
  ggplot() + 
    # add bars w/ colors for each year
    geom_bar(data = gdpr, mapping = 
          # sorted by price (highest on L side)
          aes(x = reorder(name, -milprice), y = milprice, 
          fill = year),stat = 'identity') + 
    # add total # of violations above each
    geom_text(data = gdpr2,
          mapping = aes(x = reorder(name, -milprice2), y = milprice2,
          label = numfines2), vjust = -0.5) +
    # make color scale manual values
    scale_fill_manual('Year', values = c('#D3D3D3', '#494949', '#B2D35B')) + 
    labs(title = 'GDPR Violation Enforcement Fines by EU Country',
        subtitle = 'Values above bars are total violations 2018 - 2020',
        caption = 'Source: Tidy Tuesday - R for Data Science') +
    xlab('Country where violation was enforced') + 
    ylab('Fines in millions of Euros (€)') + 
    theme_bw() + 
    theme(text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.9,0.7),
          legend.background = element_blank()) +
    coord_cartesian(ylim = c(0,55))
    
  ggsave('GDPR_Plot_04212020.png', width = 6, height = 4)
    
    
                      

  


