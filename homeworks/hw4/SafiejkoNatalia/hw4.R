library(plotly)
library(dplyr)

df <- read.csv("HW4data.csv")


convert_price <- function(price) {
  price <- substring(price, 2)
  price <- gsub(",", ".", price)
  price <- as.numeric(price) * 1000
}

df <- df %>% mutate(sale_price = convert_price(Sale.Price.bid.price)) %>% 
  mutate(crimeRate = case_when(
    Violent.Crime.Rate < 0.21 ~ "[0,0.21)",
    0.21 <= Violent.Crime.Rate & Violent.Crime.Rate < 0.38 ~ "[0.21,0.38)",
    0.38 <= Violent.Crime.Rate & Violent.Crime.Rate < 0.54 ~ "[0.38,0.54)",
    0.54 <= Violent.Crime.Rate & Violent.Crime.Rate < 0.71 ~ "[0.54,0.71)",
    0.71 <= Violent.Crime.Rate & Violent.Crime.Rate < 0.88 ~ "[0.71,0.88)",
    0.88 <= Violent.Crime.Rate & Violent.Crime.Rate < 1.05 ~ "[0.88,1.05)",
    1.05 <= Violent.Crime.Rate & Violent.Crime.Rate < 1.22 ~ "[1.05,1.22)",
    1.22 <= Violent.Crime.Rate & Violent.Crime.Rate < 1.55 ~ "[1.22,1.55)",
    1.55 <= Violent.Crime.Rate & Violent.Crime.Rate <= 1.72 ~ "[1.55,1.72]"
    
    
  ))


plot_ly(
  data = df,
  x = ~sale_price,
  type = "histogram",
  frame = ~crimeRate
) %>%
  layout(xaxis = list(title = "Wskaźnik przęstępczości"),
         yaxis = list(title = "Licznik nieruchomości w danej cenie", range = c(0, 40)),
         title = "Zależność cen i liczby nieruchomości od wskaźnika przestępczości"
  ) %>% 
  animation_slider(currentvalue = list(prefix = "Wskaźnik przestępczości w przedziale: "))
