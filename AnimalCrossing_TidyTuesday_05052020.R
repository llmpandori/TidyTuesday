################################################################
#Title: Animal Crossing - Tidy Tuesday
#Purpose: Make a plot 
#Created by: L Pandori
#Created: 05/05/2020
#Last edited: 05/05/2020
################################################################

##### Package upload #####
# Clear workspace
rm(lists=ls())

# Load libraries
library(tidyverse) # important
library(viridis)

##### Data upload and tidy ####
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

# make 1st letter of personalities and species capitalized
# code copypasta from: https://rstudio-pubs-static.s3.amazonaws.com/408658_512da947714740b99253228f084a08a9.html

CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}
# capitalize
villagers$species <- sapply(villagers$species, CapStr)
villagers$personality <- sapply(villagers$personality, CapStr)


# Question: are certain traits associated with animals?

  villagers2 <- villagers %>%
    # find # of each spp
    group_by(species) %>%
    mutate(sppcount = length(species)) %>%
    # get count by spp & personality
    group_by(species, personality) %>%
    summarize(perscount = length(gender),
              sppcount = sppcount[1]) %>%
    # calculate proportion of eahc spp w a given trait
    mutate(Proportion = perscount/sppcount) %>%
    # order factors by highest # to lowest
    arrange(desc(perscount)) %>%
    arrange(desc(sppcount))
  
    # make factors for plotting in order they appear 
    # this wasn't getting along w my tidy code above
    villagers2$species <- factor(villagers2$species, ordered = TRUE, levels = unique(villagers2$species))
    
    villagers2$personality <- factor(villagers2$personality, ordered = TRUE, levels = unique(villagers2$personality))
    
##### Make a heatmap (are they correlated?) #####

ggplot() + 
  geom_tile(data = villagers2, 
            mapping = aes(x = personality, y = species , 
                          fill = Proportion)) + 
  xlab('Personality Trait') + 
  ylab('Villager Species') +
  labs(title = 'Animal Crossing Villager Traits') + 
  scale_fill_viridis(na.value = 'black') +
      theme_classic() + 
  theme(text = element_text(size = 12, color = 'black'),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, vjust = 1, size = 12, color = 'black'),
        axis.text.y = element_text(size = 12, color = 'black'),
        panel.background = element_rect(fill = 'black')) 
    
# save plot
    
    ggsave('Animal_Xing_Plot_05052020.png', width = 4, height = 6)


  
  

  
  
