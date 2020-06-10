################################################################
#Title: African Americans in Science - Tidy Tuesday
#Purpose: Create chord diagram displaying overlapping 
#Created by: L Pandori
#Created: 06/09/2020
#Last edited: 06/09/2020
################################################################

##### Package upload #####
library(tidyverse)
library(circlize)

# make 1st letter of personalities and species capitalized
# code copypasta from: https://rstudio-pubs-static.s3.amazonaws.com/408658_512da947714740b99253228f084a08a9.html

CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

##### Data upload ######
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

##### Goal: chord diagram of occupations #####
# Steps: 
# 1 - Get data in tidy format
# 2 - Make matrix with occupations on x/y and # of obs in middle
# 3 - Make chord diagram

##### Step 1 - Tidy data #####
science <- science %>%
  # get only occupation data
  select(name, occupation_s) %>%
  # separate by delimiter (;)
  separate(occupation_s, c('o1', 'o2', 'o3'), sep = '; ') %>%
  mutate(o1 = ifelse(o1 == 'Alminac author', 'Author',
              ifelse(o1 == 'Inventor[citation needed]', 'Inventor',
              ifelse(o1 == "Biochemist[citation needed]", 'Biochemsit', 
              ifelse(o1 == "ZoologistexplorerAnthropologist", 'Zoologist', 
              ifelse(o1 == "Woods Hole Marine Biology Institute biologist", 'Marine Biologist', o1))))))


# make first letter of occupations capitalized
science$o2 <- sapply(science$o2, CapStr)
science$o3 <- sapply(science$o3, CapStr)

# remove weird NANA values
science <- science %>%
  mutate(o2 = ifelse(o2 == 'NANA', NA, o2),
        o3 =  ifelse(o3 == 'NANA', NA, o3)) %>%
# edit occupations for conciseness
  mutate(o2 = ifelse(o2 == 'Chemist (electronics/specialty Chemicals)', 'Chemist',
              ifelse(o2 == "Biomedical Engineer[citation Needed]"  , 'Biomedical Engineer', o2)))
  

# make a list of from/to values
# first find from-to that are the same (only 1 occupation)
one <- science %>%
  filter(is.na(o2) & is.na(o3)) %>%
  group_by(o1) %>%
  summarize(count = length(name)) %>%
  # make 2nd occupation column 
  mutate(o2 = o1) %>%
  select(o1, o2, count)

# now find people with2 occupations & summarize
two <- science %>%
  filter(!is.na(o2) & is.na(o3)) %>%
  group_by(o1, o2) %>%
  summarize(count = length(name))

# now find people with 3 occupations 
three <- science %>%
  filter(!is.na(o2) & !is.na(o3)) %>%
  group_by(o2, o3) %>%
  summarize(count = length(name))

three2 <- science %>%
  filter(!is.na(o2) & !is.na(o3)) %>%
  group_by(o1, o3) %>%
  summarize(count = length(name))

three3 <- science %>%
  filter(!is.na(o2) & !is.na(o3)) %>%
  group_by(o1, o2) %>%
  summarize(count = length(name))

# make colnames same
colnames(three) <- colnames(one)
colnames(three2) <- colnames(one)
colnames(three3) <- colnames(one)
colnames(two) <- colnames(one)

# rbind all froms and tos 
fromto <- bind_rows(one, two, three, three2, three3)

# combine duplicate categories
fromto <- fromto %>% 
  group_by(o1, o2) %>%
  summarize(Freq = sum(count))
  
# transform to adjacency matrix
adj <- with(fromto, table(o1, o2))

##### Make a plot :) #####
chordDiagram(fromto, transparency = 0.5)
# it's really busy, but there are a lot of inventors :)

