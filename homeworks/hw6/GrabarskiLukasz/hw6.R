# HW6
# £ukasz Grabarski

library(dplyr)
  library(ggplot2)
  library(esquisse)

  bar <- read.csv("partydata\\bar_locations.csv")
  party <- read.csv("partydata\\party_in_nyc.csv")
  irysy <- iris

  esquisser()  

library(ggplot2)

ggplot(irysy) +
 aes(x = Sepal.Length, y = Sepal.Width, colour = Species, size = Petal.Length) +
 geom_point(shape = "circle") +
 scale_color_viridis_d(option = "plasma", direction = 1) +
 theme_minimal()

library(dplyr)
library(ggplot2)

party %>%
 filter(!(City %in% "")) %>%
 ggplot() +
 aes(x = Created.Date, fill = Location.Type) +
 geom_histogram(stat = "count", bins = 52L) +
 scale_fill_brewer(palette = "RdYlGn", 
 direction = 1) +
 labs(x = "data", y = "zg³oszenia", title = "Zg³oszenia g³o¶nych imprez w Nowym Jorku", 
 subtitle = "dla najwiêkszych dzielnic", fill = "Typ lokalizacji") +
 theme_minimal() +
 theme(plot.title = element_text(size = 20L, 
 face = "italic", hjust = 0.5)) +
 facet_wrap(vars(Borough))

