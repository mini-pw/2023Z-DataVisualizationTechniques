###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 4            ###
###########################################


library(dplyr)
library(ggplot2)
library(SmarterPoland)

countries[countries$country == "Niue", ] # https://en.wikipedia.org/wiki/Niue
sum(countries$population) # `population` jest w tysiącach

## Zadanie 1
# 1. Ograniczyć zbiór krajów do tych, których nazwa jest krótsza niż 8 znaków (nchar).
# 2. Stworzyć zmienną logarytm populacji (*1000) 
#    i posortować względem niej poziomy zmiennej kraju (forcats::fct_reorder).
# 3. Zrobić wykres słupkowy pokazujący logarytm populacji w poszczególnych krajach 
#    i zaznaczyć kolorem kontynent (wykres poziomy).


### Skale (scale)


## Osie (x & y) 


## Kolor (color & fill)


# color brewer http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
# install.packages("RColorBrewer")
library(RColorBrewer)
RColorBrewer::brewer.pal(n = 5, name = "Blues")


## Zadanie 2
# 1. Ograniczyć zbiór krajów, do tych z Azji i Europy (można wykorzystać dane z Zad1).
# 2. Policzyć stosunek współczynnika zgonów do współczynnika urodzeń.
# 3. Zrobić wykres słupkowy pokazujący logarytm populacji w poszczególnych krajach 
#    i zaznaczyć kolorem wskaźnik.



### Legenda (theme & legend)


### Koordynaty (coord)


# wykres kołowy (DANGER ZONE)
ggplot2::geom_pie()


## Zadanie 3
# 1. Stworzyć zmienną wielkość kraju, która przyjmuje K wartości w zależności
#    od podziału zmiennej populacji (np. K = 3, można wykorzystać dane z Zad1).
# 2. Zrobić wykres punktowy pokazujący zależność death.rate od birth.rate 
#    i zaznaczyć kolorem wielkość kraju.


### Panele (facet)
 

### How to plot? --->>> https://www.r-graph-gallery.com
