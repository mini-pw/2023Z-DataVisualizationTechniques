#HW4 - Michał Binda
install.packages(c("plotly", "ggplot2", "dplyr", "stringi"))
library(plotly)
library(ggplot2)
library(dplyr)
library(stringi)

df <- read.csv("/Users/michal.binda/IAD/Semestr3/RStudio/semestr2/labki/HW4/Kraggle.csv")
df <- na.omit(df)


df %>% 
  mutate(Sale.Date = stringr::word(df$Sale.Date, 1)) %>% 
  filter(Sale.Date %in% c("August", "September", "October")) %>% 
  filter(yearBuilt > 1900) %>% 
  filter(!is.numeric(bathrooms)) %>% 
  plot_ly(x = ~yearBuilt,
          y = ~ Sheriff.Cost,
          type = "scatter",
          mode = "markers",
          frame = ~bedrooms,
          xlim = c(1900,2100)) %>% 
  layout(title = "Zaleznosc miedzy rokiem Budowy, a kosztem",
         xaxis = list(title = "rok Budowy"),
         yaxis = list (title = "Koszt",
                       plot_bgcolor = "#c7daec"))
          
#Wykres można dostosowac wzgledem liczby sypialni w mieszkaniu, 


