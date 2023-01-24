library(showtext)

font_add(family = "Gotham",
         regular = "data/Gotham-Black.otf",
         bold = "data/Gotham-Bold.otf",
         italic = "data/GothamBoldItalic.ttf",
)

showtext_auto()



green_colors <- c('#edf8e9',
                  '#c7e9c0',
                  '#a1d99b',
                  '#74c476',
                  '#31a354',
                  '#006d2c')

scale_fill_spotify <- function(...){
  library(scales)
  continuous_scale("fill","spotify",manual_pal(values = green_colors), ...)
  
}
?manual_pal
scale_colour_spotify <- function(...){
  library(scales)
  discrete_scale("colour","spotify",manual_pal(values = green_colors), ...)
  
}
?manual_pal
theme_spotify_base <- function(base_size=14, base_family="Gotham") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold", colour = '#1DB954',
                                      size = rel(2), hjust = 0.5),
            plot.subtitle = element_text(colour = '#1DB954', hjust = 0.5),
            
            text = element_text(),
            
            panel.background = element_rect(colour = NA, fill = '#171616'),
            
            plot.background = element_rect(colour = NA, fill = '#121212'),
            
            panel.border = element_rect(colour = NA),
            
            axis.title = element_text(face = "bold",size = rel(1.2), colour = 'white'),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(colour = 'grey70'), 
            axis.line.x = element_line(colour="grey70"),
            axis.line.y = element_line(colour="grey70"),
            axis.ticks = element_line(colour="grey70"),
            
            panel.grid.major = element_line(colour="#363333"),
            panel.grid.minor = element_blank(),
            
            legend.background = element_rect(fill ='#2b2a2a', color = 'white', linetype=7),
            legend.text = element_text(color = 'white'),
            legend.key = element_rect(fill = '#363434'),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.box = "vetical",
            legend.key.size = unit(0.8, "cm"),
            legend.title = element_text(size = 16, colour = 'white', margin = margin(0, 10,0,0)),
            
            plot.margin=unit(c(10,5,5,5),"mm"),
            
            strip.background=element_rect(colour="#2D3A4C",fill="#2D3A4C"),
            strip.text = element_text(face="bold", colour = 'white')
    )) 
}

theme_spotify_calendar <- function(base_size=14, base_family="Gotham") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold", colour = '#1DB954',
                                      size = rel(2), hjust = 0.5),
            plot.subtitle = element_text(colour = '#1DB954', hjust = 0.5),
            
            text = element_text(),
            
            panel.background = element_rect(colour = NA, fill = '#121212'),
            
            plot.background = element_rect(colour = NA, fill = '#121212'),
            
            panel.border = element_rect(colour = NA),
            
            axis.title = element_text(face = "bold",size = rel(1.2), colour = '#121212'),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(colour = '#121212'), 
            axis.line.x = element_line(colour='#121212'),
            axis.line.y = element_line(colour='#121212'),
            axis.ticks = element_line(colour='#121212'),
            
            panel.grid.major = element_line(colour='#121212'),
            panel.grid.minor = element_blank(),
            
            legend.background = element_rect(fill ='#2b2a2a', color = 'white', linetype=7),
            legend.text = element_text(color = 'white'),
            legend.key = element_rect(fill = '#363434'),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.box = "vetical",
            legend.key.size = unit(0.8, "cm"),
            legend.title = element_text(size = 16, colour = 'white', margin = margin(0, 10,0,0)),
            legend.margin = margin(5, 25, 5, 25),
            plot.margin=unit(c(10,5,5,5),"mm"),
            legend.key.width = unit(1, 'cm'),
            
            strip.background=element_rect(colour='#121212',fill='#121212'),
            strip.text = element_text(face="bold", colour = '#1DB954', size=18)
    )) 
}  
?element_text()
