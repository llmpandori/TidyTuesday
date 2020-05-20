################################################################
#Title: Beach Volleyball - Tidy Tuesday
#Purpose: 
#Created by: L Pandori
#Created: 05/19/2020
#Last edited: 05/19/2020
################################################################
##### Package upload #####

# Load libraries
library(tidyverse)
library(hexbin)

##### Data upload ####
vb <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

#### Goals #####
# Create 4-panel plot (male and female height vs aces and kills)
# Steps: 
  # 1 - Select columns of interest
  # 2 - Convert from wide (2v2) to long format
  # 3 - Conduct regression analyses
  # 4 - Plot results (points = volleyballs?)

##### Tidy #####

vb <- vb %>%
  # select columns of interest
  select(gender, w_p1_hgt, w_p1_tot_kills, w_p1_tot_aces,
         w_p2_hgt, w_p2_tot_kills, w_p2_tot_aces,
         l_p1_hgt, l_p1_tot_kills, l_p1_tot_aces,
         l_p2_hgt, l_p2_tot_kills, l_p2_tot_aces) %>%
  # add genders for each player
  mutate(w_p1_gender = gender,
         w_p2_gender = gender,
         l_p1_gender = gender,
         l_p2_gender = gender) %>%
  # remove original gender column
  select(-c(gender)) %>%
  # reorder
  select(w_p1_gender, w_p1_hgt, w_p1_tot_kills, w_p1_tot_aces,
          w_p2_gender, w_p2_hgt, w_p2_tot_kills, w_p2_tot_aces,
          l_p1_gender, l_p1_hgt, l_p1_tot_kills, l_p1_tot_aces,
          l_p2_gender, l_p2_hgt, l_p2_tot_kills, l_p2_tot_aces)

# Convert from wide to long format

# select columns of interest and add player ID column
vbw1 <- vb %>%
        select(gender = w_p1_gender, height = w_p1_hgt, 
               kills = w_p1_tot_kills, aces = w_p1_tot_aces) %>%
        mutate(player = 'w1')
        

vbw2 <- vb %>%
        select(gender = w_p2_gender, height = w_p2_hgt, 
               kills = w_p2_tot_kills, aces = w_p2_tot_aces) %>%
      mutate(player = 'w2')

vbl1 <- vb %>%
        select(gender = l_p1_gender, height = l_p1_hgt, 
               kills = l_p1_tot_kills, aces = l_p1_tot_aces) %>%
        mutate(player = 'l1')

vbl2 <- vb %>%
        select(gender = l_p2_gender, height = l_p2_hgt, 
               kills = l_p2_tot_kills, aces = l_p2_tot_aces) %>%
        mutate(player = 'l2')

# rbind
vb <- rbind(vbw1, vbw2, vbl1, vbl2)

# clean environment
remove(vbl1, vbl2, vbw1, vbw2)

# For W & M, get #aces/peron and #kills/person
vb_summary <- vb %>%
  filter(!is.na(height)) %>%
  group_by(gender, 
           height = cut(height, breaks = seq(60, 85, 5))) %>%
  summarize(number = length(gender),
            aces = sum(aces, na.rm = TRUE)/number,
            kills = sum(kills, na.rm = TRUE)/number)

vb_summary2 <- vb %>%
  filter(!is.na(height)) %>%
  group_by(gender, height) %>%
  summarize(number = length(gender),
            aces = sum(aces, na.rm = TRUE)/number,
            kills = sum(kills, na.rm = TRUE)/number)

##### Linear regressions between height and kill rate  #####

summary(lm(kills ~ height, data = filter(vb_summary2, gender == 'M')))
summary(lm(kills ~ height, data = filter(vb_summary2, gender == 'W')))

##### Plot ####

ggplot() +
  geom_point(data = filter(vb_summary2, gender == 'M'), 
              mapping = aes(x = height, y = kills),
              color = 'cadetblue3') + 
  # no significant linear relationship for women
  geom_point(data = filter(vb_summary2, gender == 'W'),
              mapping = aes(x = height, y = kills),
              color = 'darkseagreen3')+ 
  ylab('Kill Proportion (kills/players)') + 
  xlab('Height (inches)') + 
  # significant linear relationship for men
  geom_smooth(data = filter(vb_summary2, gender == 'M'), 
              mapping = aes(x = height, y = kills),
              method = 'lm', color = 'cadetblue4') + 
  # no significant linear relationship for women
  geom_smooth(data = filter(vb_summary2, gender == 'W'),
              mapping = aes(x = height, y = kills),
              method = 'lm', color = 'darkseagreen4', 
              linetype = 'longdash') + 
  geom_text(mapping = aes(x = 60.5, y = 10, 
          label = 'Men: R2 = 0.45, p < 0.01'), hjust = 0, 
          color = 'cadetblue4') +
  geom_text(mapping = aes(x = 60.5, y = 11, 
          label = 'Women: R2 = 0.10, p = 0.10'), hjust = 0,
          color = 'darkseagreen4') + 
  geom_point(mapping = aes(x = 60, y = 11),
             color = 'darkseagreen4') + 
  geom_point(mapping = aes(x = 60, y= 10), 
             color = 'cadetblue4') +
  coord_cartesian(xlim = c(60, 85)) + 
  labs(title = 'Volleyball Player Height and Kills', 
          subtitle = 'Height is related to proportional kills for men but not for women', 
          caption = 'Data source: Tidy Tuesday, Adam Vagnar, FIVB, AVB') +
  theme_modern_rc()

ggsave('Volleyball_Plot_05192020.png', width = 6, height = 4)




