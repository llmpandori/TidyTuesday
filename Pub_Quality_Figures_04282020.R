################################################################
#Title: Making Publication Quality Figures in R
#Purpose: Demonstrate process for Bracken Lab Meeting 4/29/2020
#Created by: L Pandori
#Created: 04/28/2020
#Last edited: 04/28/2020
################################################################

# Note: I have emailed you data for a paper that hasn't been published yet -- be mindful not share with anyone or post anywhere. Thank you!

##### Packages #####
# Clear workspace
rm(lists=ls())

# Load libraries
library(tidyverse) # important
library(lubridate) # date/time wrangling
library(cowplot) # plotting
library(patchwork) # multi-panel plots
library(ggThemeAssist) # help format plots
library(RColorBrewer) # nice color palettes
library(hrbrthemes) # nice themes
library(tvthemes) # fun themes

##### Load and tidy data #####

# Daily max temperature data at Crystal Cove
dailytemp_ccsp <- read_csv('dailytemp_ccsp.csv')

  # Order habitats, tide heights, then sort
  dailytemp_ccsp$place <- ordered (dailytemp_ccsp$place, 
                       levels = c('Solitary','Aggregate',
                                  'Shelter', 'Tide pool'))
  
  # Order tide heights
  dailytemp_ccsp$tideht <- ordered (dailytemp_ccsp$tideht, 
                          levels = c('High', 'Mid', 'Low'))
  
  # Arrange by ordered factors
  dailytemp_ccsp <- arrange(dailytemp_ccsp, tideht, place)

# Daily max temp summary data at Crystal Cove
dailytemp_place_summary_ccsp <- read_csv('dailytemp_place_summary_ccsp.csv')

# Order habitats, tide heights, then sort
dailytemp_place_summary_ccsp$place <- 
  ordered(dailytemp_place_summary_ccsp$place,
          levels = c('Solitary','Aggregate',                                            'Shelter', 'Tide pool'))

# Order tide heights
dailytemp_place_summary_ccsp$tideht<-ordered(dailytemp_place_summary_ccsp$tideht,
                                    levels = c('High', 'Mid', 'Low'))

# Arrange by ordered factors
dailytemp_place_summary_ccsp <- arrange(dailytemp_place_summary_ccsp, tideht, place)


##### Make a Rough Draft Figure #####

# Boxplot
ggplot() + 
  # boxplot of daily max temps across habitats (place)
  stat_boxplot(data = dailytemp_ccsp,
               mapping = aes(x = place, y = movavg.max)) +
  # make panels for each tide height
  facet_wrap(~tideht) 

# Violin plot
ggplot() + 
  geom_violin(data = dailytemp_ccsp,
              mapping = aes(x = place, y = movavg.max)) + 
  facet_wrap (~ tideht)

# Modified boxplot
ggplot() + 
  geom_crossbar(data = dailytemp_place_summary_ccsp,
                mapping = aes(x = place, y = avg,
                              ymax = max, ymin = min)) + 
  facet_wrap(~ tideht)

# What do you like about each of these plots? What do you not like?
  # I like showing distribution (tested w/ KW test) - like violin
  # I like showing max, min, mean/median - like boxplot or crossbar

# Can combine these attributes (plot 2 datasets on 1 plot - use layers)

roughdraft <- ggplot() + 
  # first argument specified will go in background
  geom_violin(data = dailytemp_ccsp,
              mapping = aes(x = place, y = movavg.max)) +
  # second argument will go in the foreground
  geom_crossbar(data = dailytemp_place_summary_ccsp,
                mapping = aes(x = place, y = avg,
                              ymax = max, ymin = min)) +
  # add letters from multiple comparisons stats
  geom_text(data = dailytemp_place_summary_ccsp, 
            mapping = aes(x = place, y = max, label = Letter),
            vjust = -0.5) +
  # wrap (create panels) after specifying datasets
  facet_wrap(~ tideht) 

roughdraft

##### Specifying colors #####

colorplot <- ggplot() + 
  # first argument specified will go in background
  geom_violin(data = dailytemp_ccsp,
              mapping = aes(x = place, y = movavg.max,
                            # fill in violins 
                            fill = place)) +
  # second argument will go in the foreground
  geom_crossbar(data = dailytemp_place_summary_ccsp,
                mapping = aes(x = place, y = avg,
                              ymax = max, ymin = min)) +
  # add letters from multiple comparisons stats
  geom_text(data = dailytemp_place_summary_ccsp, 
            mapping = aes(x = place, y = max, label = Letter),
            vjust = -0.5) +
  # wrap (create panels) after specifying datasets
  facet_wrap(~ tideht) + 
  # scale_fill_manual tells what colors for factor levels
   scale_fill_manual(values = c('#D3D3D3', '#494949', '#B2D35B', 'steelblue4')) 
  # #'s are hex colors (https://coolors.co/)
  # Named colors (http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf)

colorplot

  # Color palette can specify 
  # ex) R Color Brewer - http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
  colorplot + scale_fill_brewer(palette = 'Set1') 
  colorplot + scale_fill_spongeBob(n = 4)

##### Edit Axis Labels #####

  axisplot <- ggplot() + 
    # first argument specified will go in background
    geom_violin(data = dailytemp_ccsp,
                mapping = aes(x = place, y = movavg.max,
                              # fill argument tells what to color by
                              fill = place)) +
    # second argument will go in the foreground
    geom_crossbar(data = dailytemp_place_summary_ccsp,
                  mapping = aes(x = place, y = avg,
                                ymax = max, ymin = min)) +
    # add letters from multiple comparisons stats
    geom_text(data = dailytemp_place_summary_ccsp, 
              mapping = aes(x = place, y = max, label = Letter),
              vjust = -0.5) +
    # wrap (create panels) after specifying datasets
    facet_wrap(~ tideht) + 
    # scale_fill_manual tells what colors for factor levels
    scale_fill_manual(values = c('#D3D3D3', '#494949', '#B2D35B', 'steelblue4')) +
  
    # edit x and y axis labels 
    xlab('Tide height and habitat') + 
    ylab ('Temperature (°C)') 
  
  axisplot
  
  # alternately, you could use the code
  colorplot + xlab('Tide height and habitat') + ylab('Temperature (°C)')
  
##### Add a title, subtitle, caption #####
  
  ccsp_habitat_caption <- 'Figure 2. Daily maxima of 6-hour moving average emersed temperatures differ across most tide height and habitat combinations at Crystal Cove State Park (Kruskal-Wallis Tests followed by pairwise Post-Hoc Dunn Tests).Data were gathered from temeprature loggers placed in habitat mimics deployed in the intertidal September 2017 - August 2018. See Tables S2 and S3 for all results.'
  
  captionplot <- ggplot() + 
    # first argument specified will go in background
    geom_violin(data = dailytemp_ccsp,
                mapping = aes(x = place, y = movavg.max,
                              # fill argument tells what to color by
                              fill = place)) +
    # second argument will go in the foreground
    geom_crossbar(data = dailytemp_place_summary_ccsp,
                  mapping = aes(x = place, y = avg,
                                ymax = max, ymin = min)) + 
    # add letters from multiple comparisons stats
    geom_text(data = dailytemp_place_summary_ccsp, 
              mapping = aes(x = place, y = max, label = Letter),
              vjust = -0.5) +
    # wrap (create panels) after specifying datasets
    facet_wrap(~ tideht) + 
    # scale_fill_manual tells what colors for factor levels
    scale_fill_manual(values = c('#D3D3D3', '#494949', '#B2D35B', 'steelblue4')) +
    
    # edit x and y axis labels 
    xlab('Tide height and habitat') + 
    ylab ('Temperature (°C)') +
    
    # add caption
    labs(caption = str_wrap(ccsp_habitat_caption)) + 
    # add title 
    labs(title = 'Thermal Maxima Differ among Tide Heights & Habitats')
         # I don't want a subtitle, but you could do it this way
         #, subtitle = 'XX')
  
    captionplot
    
    # You could also make this plot using
    axisplot +     
      labs(caption = str_wrap(ccsp_habitat_caption)) + 
      labs(title = 'Thermal Maxima Differ among Tide Heights 
           & Habitats')
    
##### Add a Theme #####
    
    # explore different themes
    captionplot + theme_bw() # my fav
    captionplot + theme_cowplot()
    captionplot + theme_ft_rc() # friend fav
    captionplot + theme_spongeBob() # goofy
    
    # there's a billion of them
    # https://rfortherestofus.com/2019/08/themes-to-improve-your-ggplot-figures/
    # https://github.com/hrbrmstr/hrbrthemes
    
    # Ok...but there's still some things that aren't polished
        # X labels are crowded
        # strange text sizes
        # don't need legend
        # etc.
    
    # Let's roll with theme_bw() and change some elements of it

    themeplot <- captionplot + theme_bw() 

##### Polish theme elements #####
    # google what you want and look at example code
    # this is highly specific
    
    finalplot <- themeplot + 
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
    
    finalplot 
##### Final adjustments - look at scale #####
    
    finalplot <- finalplot + 
      coord_cartesian(ylim = c(8,40))
    
    finalplot
    
##### Saving your plot #####
    # can make in units for journal (ex - 6" x 6")
    ggsave('Exampleplot.png', #filename
           width = 6, height = 4, # dimensions
           units = 'in', # units in inches
           dpi = 300) # dpi specified by journal
    