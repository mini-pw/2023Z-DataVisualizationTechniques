
library(plotly)
library(dplyr)

#Wczytujê dane

df <- read.csv("Properties_philly_Kraggle_v2.csv")
head(df, 20)

#Wybieram odpowiednie dane

cena<-df$Sale.Price.bid.price
cena<-substring(cena, 2)
cena<-sub(",", ".", cena)
cena<-as.numeric(cena)*1000
metraz<-df$finished...SqFt.
metraz<-as.numeric(metraz)
metraz
cena_za_sq<-cena/metraz
cena_za_sq

rating<-df$Violent.Crime.Rate
rating<-as.numeric(rating)



#Tworzê pomocnicz¹ ramkê danych

df2<-data.frame(cena_za_sq, rating)
df2<-df2 %>%
  filter (cena_za_sq<=200) %>% mutate (rating=case_when(
  rating > 0 & rating <= 0.25 ~ "(0.00, 0.25>",
  rating > 0.25 & rating <= 0.5 ~ "(0.25, 0.50>",
  rating > 0.50 & rating <= 0.75 ~ "(0.50, 0.75>",
  rating > 0.75 & rating <= 1 ~ "(0.75, 1.00>",
  rating > 1 & rating <= 1.25 ~ "(1.00, 1.25>",
  rating > 1.25 & rating <= 1.5 ~ "(1.25, 1.50>",
  rating > 1.50 ~ "(1.50, +)"
))
df2

#Tworzê interaktywny wykres

fig<-plot_ly(
  data = df2,
  x = ~cena_za_sq,
  frame = ~rating,
  type = "histogram",
  xbins=list(size=10)
) %>% 
layout(
  title = "Zale¿noœæ miêdzy cen¹ za stopê kwadratow¹ nieruchomoœci a wskaŸnikiem brutalnej przestêpczoœci",
  xaxis = list(title = "Cena za stopê kwadratow¹ nieruchomoœci w $"),
  yaxis = list(title = "Liczba nieruchomoœci", range = c(0, 43))
)

fig %>% 
  animation_opts(1000) %>%
  animation_button(x = 0.00, y = 0.05, label = "Aniamcja") %>%
  animation_slider(currentvalue = list(prefix = "WskaŸnik przestêpczoœci: ", font = list(size=15, color="black")))  


