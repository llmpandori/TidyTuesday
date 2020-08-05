################################################################
# Title: European Energy
# Purpose: Circular bar plot
# Created by: L Pandori
# Created: 08/04/2020
# Last edited: 08/04/2020
################################################################

##### package upload #####
library(tidyverse) # it is Tuesday :)
library(hrbrthemes) # cool dark field themes
library(PNWColors) # color palette

##### get data & tidy #####
energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')


# tidy dataset
energy <- energy_types %>%
  # filter for level 1 only (level 1's add up to total)
  filter(level == 'Level 1') %>%
  # get rid of country = NA
  filter(!is.na(country_name)) %>%
  # select columns of interest
  select(country, country_name, type, `2018`) %>%
  # make easier column name
  rename(gwh2018 = `2018`, Type = type)

# prelimnary plot
ggplot(data = energy, 
       # fill color by type of energy
       mapping = aes(x = country_name, y = gwh2018, fill = Type)) +
  # column plot
  geom_col() +
  # x, y and title labels
  xlab('Country') +
  ylab('Energy in GWh (Gigawatt Hours)') +
  ggtitle('Energy Type by European Country') +
  coord_polar()

# prelim plot is too busy - condense by looking at top 10 highest energy producing countries vs bottom 10?

energy_summary <- energy %>%
  group_by(country_name) %>% 
  summarize(total = sum(gwh2018)) %>%
  arrange(desc(total))

# get top and bottom 10
  high10 <- top_n(energy_summary,10, total)$country_name
  low10 <- top_n(energy_summary, -10, total)$country_name


# sort and filter by top 10
high10data <- energy %>%
  filter(country_name %in% high10) %>%
  factor(high10data$country_name, ordered = TRUE, levels %in% high10)
 
# try plot again with high/top 10
  ggplot(data = high10data, 
         # fill color by type of energy
         mapping = aes(x = reorder(country_name, desc(gwh2018)), 
                       y = gwh2018, fill = Type)) +
    # column plot
    geom_col() +
    # x, y and title labels
    xlab('Top 10 Countries, Source: Eurostat, #TidyTuesday by T. Mock') +
    ylab('Energy in GWh') +
    ggtitle('Energy Sources across European Countries') +
    coord_polar() +
    scale_fill_manual(values = pnw_palette( name= 'Cascades', n = 7, type= 'continuous')) +
    theme_ft_rc() +
    theme(
      text = element_text(size = 14, color = 'white'),
      axis.text = element_text(size = 18, color = 'white'),
      axis.title.y = element_text(size = 10, color = 'white'),
      )

  ggsave('energy_tidytues.png', height = 8, width = 8, dpi = 300)




  







