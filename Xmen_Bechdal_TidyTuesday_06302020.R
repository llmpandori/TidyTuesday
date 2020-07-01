################################################################
#Title: X Men Tidy Tuesday
#Purpose: have fun
#Created by: L Pandori
#Created: 06/30/2020
#Last edited: 06/30/2020
################################################################

##### Package upload #####
library(tidyverse)
library(cowplot) # working with images
library(magick)

##### Data upload & tidy #####
xmen_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/xmen_bechdel.csv')

##### Plot - Proportion of Bechdel-test passing X-men issues #####

# tidy 
bechdel <- tibble(
  Answer = c('Yes','No'),
  pass = c(nrow(filter(xmen_bechdel, pass_bechdel == 'yes')),
                nrow(filter(xmen_bechdel, pass_bechdel == 'no'))))

# calculate things to plot (thank you R Graph Gallery)
bechdel <- bechdel %>%
  mutate(fraction = pass/sum(pass),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)))

# Make the plot
baseplot <- ggplot(bechdel, 
  aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Answer))+
  scale_fill_manual(values=c('brown1', 'gold')) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(0.35, 4)) + 
  theme_void() +
  theme(panel.background = 
          element_rect(fill = 'gray90', color = 'gray90'),
        plot.background = element_rect(fill = 'gray90', color = 'gray90'))


# Put x men logo (from wikimedia commmons) on plot
ggdraw() +
  draw_plot(baseplot)+
  draw_image('xmenlogo.png', x = 1, hjust = 1.184, 
             vjust = -0.127, width = 0.8, height = 0.8) +
  draw_label(label = 'Do issues of', x = 0.45, y = 0.62,  fontface = "plain", color = "black", size = 14,
             angle = 0, lineheight = 0.9, alpha = 1) +
  draw_label(label = 'pass the Bechdel test?', x = 0.45, y = 0.38,  fontface = "plain", color = "black", size = 14,
             angle = 0, lineheight = 0.9, alpha = 1)

ggsave('xmenplot.png')





