setwd("/Users/mat/Desktop/twd/2023Z-DataVisualizationTechniques/homeworks/hw4/NizwantowskiMateusz")
df <- read.csv("Properties_philly_Kraggle_v2.csv")

library(plotly)
library(dplyr)
library(readr)


# Jako ze nienawidze car culture, z ktorego slynie Ameryka 
# i jestem zapalonym czlonkiem https://www.reddit.com/r/fuckcars/
# to zbadam czy w tym piekle na ziemi (Stany Zjednoczone Ameryki)
# istnieje jakies powiazanie pomiedzy cena/ cena za metr (jeszcze nie wiem)
# a Avg.Walk.Transit.score, wiem ze to bardzo basic ale 
# a) to jest zadanie za 5 pkt  b) jestem ciekawy wynikow



dane <- df %>%
  filter(Sale.Price.bid.price != "") %>%
  filter(finished...SqFt. > 2) %>%
  select(Sale.Price.bid.price, Avg.Walk.Transit.score, finished...SqFt.) %>%
  mutate(Sale.Price.bid.price = parse_number(Sale.Price.bid.price)) %>%
  mutate(cena_za_stope = Sale.Price.bid.price / finished...SqFt.)



hist(dane$Sale.Price.bid.price, breaks=20)
hist(dane$cena_za_stope, breaks=20)



dane <- dane %>%
  mutate(kwantyle_cena_za_stopa = ntile(cena_za_stope, 5)) %>%
  mutate(kwantyle_cena_za_stopa = case_when(kwantyle_cena_za_stopa == 1 ~ "Najtansze 20%",
                                            kwantyle_cena_za_stopa == 2 ~ "Pomiedzy 20% a 40%",
                                            kwantyle_cena_za_stopa == 3 ~ "Pomiedzy 40% a 60%",
                                            kwantyle_cena_za_stopa == 4 ~ "Pomiedzy 60% a 80%",
                                            kwantyle_cena_za_stopa == 5 ~ "Najdrozsze 20%",
                                            TRUE ~ "Other")) %>%
  mutate(kwantyle_cena_za_stopa = factor(kwantyle_cena_za_stopa, levels = c("Najtansze 20%", "Pomiedzy 20% a 40%", "Pomiedzy 40% a 60%", "Pomiedzy 60% a 80%", "Najdrozsze 20%")))

  

plot_ly(
  data = dane, 
  y = ~Sale.Price.bid.price, 
  x = ~Avg.Walk.Transit.score, 
  color = ~kwantyle_cena_za_stopa,
  colors = "Set1"
) %>%
  layout(title = 'Cena posiadlosci a wskaznik Avg.Walk.Transit.score', plot_bgcolor = "#e5ecf6", 
         yaxis = list(title = 'Price [$]'), legend = list(title=list(text='Cena za ft^2')))

# Podsumowując to nic z tego wykresu nie wynika ale jest interaktywny
# do najladniejszych tez nie nalezy 
  
