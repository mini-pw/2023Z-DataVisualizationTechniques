setwd()

library(dplyr)
library(plotly)

options(stringsAsFactors = TRUE)

df <- read.csv("Properties_philly_Kraggle_v2.csv")
na.omit(df) -> df


df %>%
  select(yearBuilt, Opening.Bid, Violent.Crime.Rate,Sale.Date ,Zillow.Address, School.Score) %>%
  filter(yearBuilt > 0) %>%
  mutate(yearBuilt = yearBuilt%/%10 * 10) %>%
  filter(yearBuilt >= 1900) -> df

df %>%
  mutate(Sale.Date = stringr::word(df$Sale.Date, 1)) %>%
  filter(Sale.Date %in% c("August", "September")) %>%
  mutate(Sale.Date = ifelse(Sale.Date == "August", "Sierpień", "Wrzesień")) -> df

fonts <- list(
  famil = "sans serif",
  size = 14,
  color = "black"
)

wykres <- df %>%
  plot_ly(x = ~School.Score, 
          y = ~Violent.Crime.Rate,
          text = ~Zillow.Address,
          hoverinfo = "text", 
          size = ~Opening.Bid+2,
          frame = ~yearBuilt,
          type = 'scatter',
          mode = "markers",
          color = ~Sale.Date,
          colors = "Set1",
          legendgroup = ~Sale.Date,
          showlegend = TRUE) %>%
  animation_slider(currentvalue = list(prefix = "Dekada wybudowania: ")) %>%
  layout(title = "Wykres zależności wskaźnika przestępczości\n w miejscu sprzedaży domu od wskaźnika jakości kształcenia\n(dane z wakacji 2016)",
         subtitle = "Dane z wakacji roku 2016",
         xaxis = list(title = "Wskaźnik jakości szkolnictwa"), 
         yaxis = list(title = "Wskaźnik przestępczości"),
         legend = list(x = 1,
                       y = 0.9,
                       title = list(text = "Miesiąc zakupu")),
         font = fonts)
wykres
