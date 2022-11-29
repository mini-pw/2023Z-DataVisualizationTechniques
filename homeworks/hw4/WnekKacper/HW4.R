library(plotly)
library(dplyr)
# Pobranie danych Philadelphia Real Estate
data <- read.csv("prop.csv")

# Zmodyfikowanie danych (głównie pozbycie się pustych pól/nulli itp)
data <- data %>% 
  filter(yearBuilt>1900 & yearBuilt<2000) %>% 
  filter(Opening.Bid>0) %>% 
  filter(bedrooms>0) %>%
  mutate(price_case = case_when(
    Opening.Bid>0 & Opening.Bid<=2000 ~ "(0;2000]",
    Opening.Bid>2000 & Opening.Bid<=4000 ~ "(2000;4000]",
    Opening.Bid>4000 & Opening.Bid<=6000 ~ "(4000;6000]",
    Opening.Bid>6000 & Opening.Bid<=8000 ~ "(6000;8000]",
    Opening.Bid>8000 & Opening.Bid<=10000 ~ "(8000;10000]",
    Opening.Bid>10000 & Opening.Bid<=12000 ~ "(10000;12000]",
    Opening.Bid>12000 & Opening.Bid<=14000 ~ "(12000;14000]",
    Opening.Bid>14000 & Opening.Bid<=16000 ~ "(14000;16000]",
    Opening.Bid>16000 & Opening.Bid<=18000 ~ "(16000;18000]",
    Opening.Bid>18000 & Opening.Bid<=20000 ~ "(18000;20000]",
    Opening.Bid>20000 ~ ">20000")
    ) %>% 
  #Ustawienie w taki sposób, aby wszystko sie zgadzalo na osi x na wykresie
  mutate(price_case=factor(price_case,levels = c("(0;2000]","(2000;4000]",
                                                 "(4000;6000]","(6000;8000]","(8000;10000]",
                                                 "(10000;12000]","(12000;14000]","(14000;16000]","(16000;18000]",
                                                 "(18000;2000]",">20000"))) %>% 
  #Przygotowanie ramki z pogrupowanymi latami
  mutate(year=case_when(
    yearBuilt >= 1900 & yearBuilt < 1920 ~ "1900 - 1919",
    yearBuilt >= 1920 & yearBuilt < 1940 ~ "1920 - 1939",
    yearBuilt >= 1940 & yearBuilt < 1960 ~ "1940 - 1959",
    yearBuilt >= 1960 & yearBuilt < 1980 ~ "1960 - 1979",
    yearBuilt >= 1980 & yearBuilt < 2000 ~ "1980 - 1999",
    
  ))  
plot_ly(
  data=data,
  x=~price_case,
  type= "histogram",
  frame= ~bedrooms,
  color= ~year
) %>%
  layout(
    title = "Wykres liczby dostępnych nieruchomości w zależnosci od liczby 
         pokoi oraz oferty otwarcia, z podziałem na lata",
    xaxis = list(title = "oferta otwarcia"),
    yaxis = list(title = "Liczba nieruchomości", range = c(0, 70)),
    legend=list(title=list(text="Lata"))
         
  ) %>% 
  animation_slider(currentvalue = list(prefix = "Liczba pokoi "))
 
