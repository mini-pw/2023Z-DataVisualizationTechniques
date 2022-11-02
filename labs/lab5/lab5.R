###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 5            ###
###########################################

library(ggplot2)
library(dplyr)
library(SmarterPoland)

## Zadanie 1
# Z zamieszczonego pliku .pdf w folderze lab5 należy rozwiązać jedno z dwóch zadań. 
# Dane potrzbne do odtowrzenia wizualizacji wczytujemy następująco:

df <- read.csv(file = "https://raw.githubusercontent.com/R-Ladies-Warsaw/PoweR/master/Cz%C4%99%C5%9B%C4%87%202%20-%20Formatowanie%20danych/R/data/ranking.csv", 
               encoding = "UTF-8")


## patchwork

library(patchwork)

# Więcej: https://ggrepel.slowkow.com/articles/examples.html


## Zadanie 2
# Narysuj wykres punktowy zależności między wskaźnikiem urodzeń a wskaźnikiem śmierci 
# oraz podpisz punkty o najniższym i najwyższym wskaźniku śmiertelności (nazwą kraju).


## Zadanie 3 - stworzyć wykres gęstości brzegowych:
# a) wykres punktowy dwóch wskaźników + kolor
# b) dodać po lewej rozkład zmiennej death.rate
# c) dodać na dole rozkład zmiennej birth.rate


## maps

require(maps)
?map_data

# maps::county(), maps::france(), maps::italy(), maps::nz(), maps::state(), maps::usa(), maps::world(), maps::world2()


# Więcej: https://ggplot2.tidyverse.org/reference/coord_map.html


## Zadanie 4
# Wykorzystując dane countries narysuj mapę świata i zaznacz na niej wskaźnik urodzeń. 


## ggpubr

devtools::install_github("kassambara/ggpubr")

library(ggpubr)

# Więcej: https://github.com/kassambara/ggpubr/
