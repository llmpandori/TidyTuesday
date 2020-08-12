################################################################
# Title: Avatar the Last Airbender  
# Purpose: Make something with text analytics
# Created by: L Pandori
# Created: 08/11/2020
# Last edited: 08/11/2020
################################################################

##### package upload #####
library(tidyverse)# it is Tuesday :)
library(tvthemes) # has Avatar color palette
library(tidytext) # deal w text strings elegantly
library(circlize) # make chord diagram
library(ggnetwork)
# appa package not available for 4.0.2 :( 
# will download when available (devtools?)

##### data upload #####
avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')

##### goals #####
# chord diagram for mentions of other characters by name (no nicknames other than the Avatar/Aang)

# steps
# 1 - use tidy text to make words into rows
# 2 - filter for names in character list
# 3 - mutate the Avatar into Aang mention
# 4 - tally # of mentions for each character (make matrix)
# 5 - make into chord diagram

##### tidy data #####
# designate list of characters of interest
characterlist <- c('Katara', 'Sokka', 'Zuko', 'Aang', 'Iroh', 'Azula', 'Toph', 'Avatar')

characterlist2 <- c('katara', 'sokka', 'zuko', 'aang', 'iroh', 'azula', 'toph', 'avatar')

# filter observations of interest 
avatar2 <- avatar %>%
  # select relevant columns
  select(character, character_words) %>%
  # filter for only characters of interest from list above
  filter(character %in% characterlist) %>%
  # make one word per column
  unnest_tokens(word, character_words) %>%
  # filter mentions from list 2 (lower case)
  filter(word %in% characterlist2) %>%
  # tally # of mentions per character
  group_by(character, word) %>%
  tally() %>%
  # turn into adjacency matrix
  pivot_wider(names_from = `word`, values_from = `n`, values_fill = 0) %>%
  # ungroup to add row for the avatar
  ungroup() %>%
  add_row(katara = 0, sokka = 0, zuko = 0, aang = 0, iroh = 0,
          azula = 0, toph = 0, avatar = 0, .before = 2) %>%
  # get rid of first column (with names -- will use for row names)
  select(-c(character))
  

# consistent row and column names
rownames(avatar2) <- c('Aang', 'Avatar', 'Azula', 'Iroh', 'Katara', 'Sokka', 'Toph', 'Zuko')
colnames(avatar2) <- c('Aang', 'Avatar', 'Azula', 'Iroh', 'Katara', 'Sokka', 'Toph', 'Zuko')

# make into chord diagram with circlize

chordDiagram(avatar2, 
      grid.col = scale_color_viridis_b(), transparency = 0.5,
      annotationTrack = c('name', 'grid'))







