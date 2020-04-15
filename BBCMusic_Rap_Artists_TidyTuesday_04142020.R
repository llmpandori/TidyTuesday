################################################################
#Title: BBC Music - Rap Artists
#Purpose: Make a plot 
#Created by: L Pandori
#Created: 4/14/2020
#Last edited: 4/14/2020
################################################################

##### Data prep and package upload #####
# Clear workspace
rm(lists=ls())

# Load libraries
library(tidyverse) # tidying!
library(tidytext)
library(tokenizers)
library(ggpubr)
library(fishualize)

# Data upload
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# 2 silly missions: 
  # Find most divisive tracks (most variance in score)
      # Are scores becoming more variable over time?
  # Plot gender makeup over time (proportion M/F in each decade)

##### 10 most divisive tracks #####
 # Find variance in scores
scores <- mutate(rankings, variance = var(n1:n5))

# Sort by most to least variance
scores <- arrange(scores, variance)

# Frequency of artists that have 'lil, young, boy, kid, big, junior, man, dr - by decade
  # Look at Priya's That's What She Said (The Office) Tidy Tuesday for     inspiration and help 
# format list of names
names <- tokenize_sentences(rankings$artist)
names <- lower_string <-tolower(names)
rankings2 <- cbind(rankings, names)

# search for titles of interest
namesfilter1 <- rankings2[grep('lil', rankings2$names),]
namesfilter1$chr <- 'Lil'
namesfilter2 <- rankings2[grep('young', rankings2$names),]
namesfilter2$chr <- 'Young'
namesfilter3 <- rankings2[grep('boy', rankings2$names),]
namesfilter3$chr <- 'Boy'
namesfilter4 <- rankings2[grep('kid', rankings2$names),]
namesfilter4$chr <- 'Kid'
namesfilter5 <- rankings2[grep('big', rankings2$names),]
namesfilter5$chr <- 'Big'
namesfilter6 <- rankings2[grep('junior', rankings2$names),]
namesfilter6$chr <- 'Jr'
namesfilter7 <- rankings2[grep('man', rankings2$names),]
namesfilter7$chr <- 'Man'

# rbind datasets
namedata <- rbind(namesfilter1, namesfilter2, namesfilter3, namesfilter4, namesfilter5, namesfilter6, namesfilter7)

# calculate decade
# copypasta from stack overflow : https://stackoverflow.com/questions/35352914/floor-a-year-to-the-decade-in-r
floor_decade = function(value){ return(value - value %% 10) }
namedata <- mutate(namedata, floordecade = floor_decade(year))


# Summarize by decade
namesummary <- namedata %>%
  group_by(floordecade, chr) %>%
  summarize(count = n()) %>%
  arrange(floordecade)

namesummary$Decade <- as.factor(namesummary$floordecade)
namesummary$Descriptor <- as.factor(namesummary$chr)

# Plot as stacked barplot
ggplot(data = namesummary) + 
  geom_bar(mapping = aes(x = Decade, y = count, fill = Descriptor), stat = 'identity') + 
  labs(title = 'Hip-Hop Artist Descriptors by Decade') + 
  ylab('Frequency') + 
  xlab('Decade Start') + 
  # cleaner theme
  theme_bw(base_size = 18)

ggsave('HipHopArtist_Plot_04142020.png', width = 10, height = 6)

