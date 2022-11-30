library(dplyr)
library(plotly)

df <-  read.csv("Properties_philly_Kraggle_v2.csv")
df <- na.omit(df)

zmien_na_cene <- function(cena) {
  cena <- substring(cena, 2)
  cena <- gsub(",", ".", cena)
  cena <- as.numeric(cena) * 1000
}
zmien_na_cene_do_wyswietlenia <- function(cena) {
  cena <- substring(cena, 2)
  cena <- gsub(",", " ", cena)
}

df <- df %>% 
  select(Sheriff.Cost, Violent.Crime.Rate, Sale.Date, Sale.Price.bid.price, PropType) %>% 
  mutate(cena_sold = zmien_na_cene(Sale.Price.bid.price)) %>% 
  mutate(cena_wyswietl = zmien_na_cene_do_wyswietlenia(Sale.Price.bid.price)) %>% 
  mutate(Sale.Date = stringr::word(df$Sale.Date, 1)) %>% 
  filter(Sale.Date %in% c("August", "September", "October"))

df$Sale.Date <- factor(df$Sale.Date, levels = c("August", "September", "October"))

df


wykres <- df %>% 
  plot_ly(
    x = ~Violent.Crime.Rate,
    y = ~ Sheriff.Cost,
    type = "scatter",
    mode = "markers",
    frame = ~Sale.Date,
    size = ~cena_sold,
    color = ~PropType,
    colors = "Dark2",
    text = ~paste('</br> $ House price: ', cena_wyswietl),
    hoverinfo = "text"
  ) %>% 
  animation_slider(currentvalue = list(prefix = "Current month: ")) %>% 
  layout(title = "Plot of sheriff cost depending on crime rate in the area",
         xaxis = list(title = "Crime rate"), 
         yaxis = list(title = "Sheriff cost"),
         legend = list(x = 1,
                       y = 0.8,
                       title = list(text = "Type of property:")))

wykres

#animacja na wykresu pozwala w łatwy sposób sprawdzić jak zmieniała się badana zależność w poszczególnych miesiącach 
#dodatkowo po najechaniu na dany punkt wyświetla się dokładna cena domu
#wizualnie ułatwić odczyt może również rozmiar punktu, który jest zależny od ceny
#wszystkie punkty rozróżnione kolorystycznie w zależności od typu domu
#animacja potwierdza początkowe przypuszczenia: im mniejszy koszt usług szeryfa tym większy współczynnik kryminalny i sprawdza się to w każdym miesiącu

