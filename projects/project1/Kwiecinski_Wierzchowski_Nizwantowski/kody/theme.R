library(showtext)

library(dplyr)
library(ggplot2)
library(scales)
library(tidyquant)
library(ggthemes)

font_add_google("Work Sans", family = "a")
font_add("lovelo", "lovelo/Lovelo Black.otf")
showtext_auto()


theme_form <- function(){ 
  font <- "lovelo"  #assign font family up front
  colour <-"#ffffff"
  
  theme_few() %+replace%    #replace elements we want to change
    
    theme(
      #grid elements
      panel.grid.major = element_line(color = "#dddddd",
                                      size=0.25,
                                      linetype = 1),
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      # plot elements - transparency
      plot.background = element_rect(fill='transparent', color=NA),
      legend.position = "none", 
      panel.background = element_rect(fill='transparent'),
      axis.line = element_line(color = "#dddddd"),
      panel.border = element_rect(color = colour, fill=NA),
      
      
      # text elements
      text = element_text(colour = colour, family = font,
                          margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "pt")),
      axis.text = element_text(colour = colour, family = font, size=22),
      
      #text elements
      plot.title = element_text(
        hjust=0,
        size = 35),               #raise slightly
      
      plot.subtitle = element_text( 
        hjust=0,
        size = 30),               #font size
      
      plot.caption = element_text(#font family
        size = 14),               #right align
      
      axis.title = element_text(            #font family
        size = 25),               #font size
      
      #axis.text = element_text(              #axis text
      #  family = font,            #axis famuly
      #  size = 9),                #font size
      
      #axis.text.x = element_text(            #margin for axis text
      #  margin=margin(5, b = 10))
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}
