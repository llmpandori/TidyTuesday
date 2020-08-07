################################################################
# Title: DeTide Data Tutorial
# Purpose: Separate temp data during high and low tide
# Created by: L Pandori
# Created: 08/07/2020
# Last edited: 08/07/2020
################################################################

##### package upload #####
library(tidyverse) # it is Tuesday :)

##### data upload #####
tides <- read_csv("GosportHarbor_Tides_6.23.19.csv", 
  col_types = cols(datetime = col_datetime(format = "%m/%d/%Y %H:%M")))

# tide height 2 m (high), 0.5 (low) m above MLLW

##### calculate air (1) or water (0) ####
tides <- tides %>%
  mutate(high_airwater = ifelse(tidelvl < 2, 1, 0),
         low_airwater = ifelse(tidelvl < 0.5, 1, 0),
         year = lubridate::year(datetime),
         day = lubridate::date(datetime))

byday <- tides %>%
  group_by(day) %>%
  summarize(high_emersion_day = mean(high_airwater),
            low_emersion_day = mean(low_airwater),
            year = year)

year_byday <- byday %>%
  group_by(year) %>%
  summarize(day_high_emersion = mean(high_emersion_day),
            day_low_emersion = mean(low_emersion_day))

year_byyear <- tides %>%
  group_by(year) %>%
  summarize(year_high_emersion = mean(high_airwater),
            year_low_emersion = mean(low_airwater))


#### merge by year and by day, plot side-by-side ####
emersion <- left_join(year_byyear, year_byday)

ggplot(data = emersion, mapping = aes(x = year_high_emersion, y = day_high_emersion)) + 
  geom_point() 
  

summary(lm(emersion$year_high_emersion ~ emersion$day_high_emersion))

ggplot(data = emersion, mapping = aes(x = year_low_emersion, y = day_low_emersion)) + 
  geom_point()

summary(lm(emersion$year_low_emersion ~ emersion$day_low_emersion))


##### plot low vs high #####
data <- pivot_longer(year_byday, cols = c(day_high_emersion, day_low_emersion), names_to = 'lowhigh', values_to = 'emersion')


ggplot(data = filter(data, year != 2020), mapping = aes(x = year, y = emersion, fill = lowhigh)) +
  geom_col(position = 'dodge')

