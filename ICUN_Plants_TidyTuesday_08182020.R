################################################################
# Title: ICUN Red List Plants  
# Purpose: Ridgeline plot by continent
# Created by: L Pandori
# Created: 08/18/2020
# Last edited: 08/18/2020
################################################################

##### package upload #####
library(tidyverse)# it is Tuesday :)
library(extrafont) # use fonts
library(ggridges) # ridgeline/joyplot

##### upload fonts #####
# this took a looooooong time, so it's commented out
fonts()

##### get data #####
plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')

##### tidy data #####
plants2 <- plants %>%
  # 15 plants lost when filtered for no NA's in year last seen
  filter(!is.na(year_last_seen)) %>%
  select(continent, year_last_seen) %>%
  # summarize by last seen and continent (including all combos)
  # group_by(continent, year_last_seen, .drop = FALSE) %>%
  # tally # of observations for each 
  # tally() %>%
  # get year ceiling from last seen years by taking last 4 characters
  mutate(year_ceiling = as.numeric(substr(year_last_seen, nchar(year_last_seen)-3, nchar(year_last_seen))))


plants3 <- 
  plants %>%
  # 15 plants lost when filtered for no NA's in year last seen
  filter(!is.na(year_last_seen)) %>%
  select(continent, year_last_seen) %>%
  # summarize by last seen and continent (including all combos)
  group_by(continent, year_last_seen, .drop = FALSE) %>%
  # tally # of observations for each 
  tally() %>%
  # get year ceiling from last seen years by taking last 4 characters
  mutate(year_ceiling = as.numeric(substr(year_last_seen, nchar(year_last_seen)-3, nchar(year_last_seen))))

##### make ridge line plot #####
# inspiration: r-graph-gallery.com/294-basic-ridgeline-plot.html
ggplot(data = plants2, 
       mapping = 
         aes(x = year_ceiling, y = fct_rev(continent), fill = continent)) +
  geom_density_ridges() +
  xlab('Ceiling of period last seen') +
  ylab('Continent') +
  theme(legend.position = "none") +
  ggtitle('Ridge Plot View') +
  theme_bw() +
  theme(text = element_text(size = 12),
        # no background to wrap panels
        strip.background = element_blank(),
        # panel labels outside x axis labels
        strip.placement = 'outside',
        # tilt and adjust position of x axis labels
        axis.text.y = element_text(size = 11, color = 'black'),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, size = 11, color = 'black'),
        # no legends
        legend.position = 'none',
        # make caption serif text to match journal style
        plot.caption = element_text(family = 'serif', 
                                    size = 12, color = 'black', hjust = 0))
  
ggsave('plants_ridges.png', width = 4, height = 6)

##### make faceted bar plot (since ridges aren't good to use for categorical data)

ggplot(data = plants3, mapping = aes(x = year_ceiling, y = n, fill = continent)) +
  geom_col() +
  facet_wrap(~continent, ncol = 1) +
  xlab('Ceiling of Year Last Seen') +
  ylab('Number of Plants') +
  ggtitle('Last Observations of Extinct Plants') +
  theme_bw() +
  theme(text = element_text(size = 12),
        # no background to wrap panels
        strip.background = element_blank(),
        # panel labels outside x axis labels
        strip.placement = 'outside',
        # tilt and adjust position of x axis labels
        axis.text.y = element_text(size = 11, color = 'black'),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, size = 11, color = 'black'),
        # no legends
        legend.position = 'none',
        # make caption serif text to match journal style
        plot.caption = element_text(family = 'serif', 
                                    size = 12, color = 'black', hjust = 0))
  

ggsave('plants_barplot.png', width = 4, height = 6)




