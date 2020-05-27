################################################################
#Title: Cocktails - Tidy Tuesday
#Purpose: Find out how to get the most bang for your buck
#Created by: L Pandori
#Created: 05/26/2020
#Last edited: 05/26/2020
################################################################
##### Package upload #####
library(tidyverse)
library(vegan)
library(ggdendro)

##### Upload & Tidy Data #####
cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')

cocktail_makeup <- cocktails %>%
  mutate(measure2 = 1) %>%
  select(drink, ingredient, measure2) %>%
  pivot_wider(names_from = ingredient, 
              values_from = measure2,
              values_fn = list(measure2 = length)) %>%
  replace(is.na(.), 0)

# make row names from 1st column and get rid of names
rownames(cocktail_makeup) <- cocktail_makeup$drink
cocktail_makeup$drink <- NULL
  
# plot results as dendogram
ct <- hclust(dist(cocktail_makeup))
ggdendrogram(ct, rotate = FALSE, size = 2)

# it's so messy! - filter it down to some key ingredients (I currently have)

unique(cocktails$ingredient)
  
# tequila drinks 
tequila <- cocktails %>%
  group_by(drink) %>%
  # any drink with tequila as an ingredient
  filter(any(ingredient == 'Tequila')) %>%
  mutate(measure2 = 1) %>%
  select(drink, ingredient, measure2) %>%
  pivot_wider(names_from = ingredient, 
              values_from = measure2,
              values_fn = list(measure2 = length)) %>%
  replace(is.na(.), 0)

# 
rownames(tequila) <- tequila$drink
cocktail_makeup$drink <- NULL

# plot results as dendogram
teq <- hclust(dist(tequila))

# get segments to plot
teq_dendrogram <- as.dendrogram(teq)
# Rectangular lines
teq_data <- dendro_data(teq_dendrogram, type = "rectangle")


# plot with only tequila drinks
ggplot(segment(teq_data)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_text(aes(x = x, y = y, label = label, hjust = 1), 
            data= label(teq_data)) + 
  coord_flip(ylim = c(-3,4)) +
  theme_void() +
  ggtitle('Tequila Drink Taxonomy')

# save plot
ggsave('Tequila_Taxonomy_05262020.png')

