#PRACA DOMOWA 2
#Michal Binda

library(dplyr)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
# Install
install.packages("wesanderson")
# Load
library(wesanderson)
#Wykres do poprawy:
#https://www.anychart.com/products/anychart/gallery/3D_Column_Charts/100_Stacked_3D_Column_Chart.php


#Ten wykres przedstawia wskaźnik sprzedaży dla 9 produktów kosmetycznych, które były sprzedawane w czterech stanach: Florydzie, Teksasie, Arizonie i Nevadzie.
#Do poprawy : słupki 3D są niewyrazne i nie wpływają na przekaz informacji, jeden słupek zawiera inforamcje dla 4 różnych stanów co utrudnia porównanie ich wartosci do siebie, zle dobrana kolorystyka, kolory są o podobnej barwie co utrudnia odczytanie infomacji z wykresu

# kod:
#Tworze ramke danych na posdtawie żrodła
stany <- c( "Nevada","Arizona", "Texas","Florida","Nevada","Arizona", "Texas","Florida","Nevada","Arizona", "Texas","Florida","Nevada","Arizona", "Texas","Florida","Nevada","Arizona", "Texas","Florida")
rodzaj <- c("Nail polish","Nail polish","Nail polish","Nail polish","Rouge","Rouge","Rouge","Rouge","Lipstick","Lipstick","Lipstick","Lipstick","Eyeliner","Eyeliner","Eyeliner","Eyeliner","Mascara","Mascara","Mascara","Mascara")
procent <- c(15, 20, 10, 55,17,10,28,45,28,12.5,30,29.5,12.5,10,50.5,27,37.5,13,23,26.5)


tab <- data.frame(stany, rodzaj, procent)

#Tworze wykres 
View(tab)
ggplot(tab, aes(fill = stany, y=procent, x=rodzaj)) +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 20),
          panel.background = element_rect(fill = "grey",
                                          colour = "lightblue",
                                          size = 1.5, linetype = "solid"),
          panel.grid.minor = element_line(size = 1, 
                                          linetype = "longdash")) +
    labs(title = "Regionalny wskaźnik sprzedaży produktów kosmetycznych",
         title.size = 50, y= "częstosc (%)", fill = "STANY")  +
    geom_bar(position = "dodge", stat ="identity" ) 

                     
#Poprawiony Wykres
-Mój wykres:ma podpisane osie, dzięki czemu łatwo wywnioskować co wykres przedstawia, ma również gęściej dobrane linie procentów, co pomoaga dokładniej określić wartość każdego słupka,
słupki nie są ułożone w stos, są ułożone obok siebie, więc łatwiej je do siebie porównać, legenda jest na dole, wiec nie myli się z tytułem, słupki zamienione zostały z 3D na 2D, żeby były lepeij widoczne
    