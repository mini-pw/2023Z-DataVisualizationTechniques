library(dplyr)
library(plotly)



data <- read.csv("Properties_philly_Kraggle_v2.csv")

df <- data %>% 
  filter(Address != '' ) %>% 
       
  mutate(yearBuilt = (yearBuilt %/% 10)*10, 
         Sale.Price.bid.price = as.numeric(gsub(",", ".", substring(Sale.Price.bid.price, 2))))  %>% 
  mutate(Sale.Price.bid.price =Sale.Price.bid.price * 1000) %>% 
  mutate(crime_interval = case_when(
    Violent.Crime.Rate <= quantile(data$Violent.Crime.Rate, na.rm = T, probs = seq(0, 1, 0.25))[2] ~ "[0,Q1]",
    Violent.Crime.Rate <= quantile(data$Violent.Crime.Rate, na.rm = T, probs = seq(0, 1, 0.25))[3] ~ "(Q1, Q2]",
    Violent.Crime.Rate <= quantile(data$Violent.Crime.Rate, na.rm = T, probs = seq(0, 1, 0.25))[4] ~ "(Q2, Q3]",
    T ~ "(Q3, max]"
  )) %>% 
  mutate(crime = case_when(
    Violent.Crime.Rate <= quantile(data$Violent.Crime.Rate, na.rm = T, probs = seq(0, 1, 0.25))[2] ~ 1,
    Violent.Crime.Rate <= quantile(data$Violent.Crime.Rate, na.rm = T, probs = seq(0, 1, 0.25))[3] ~ 2,
    Violent.Crime.Rate <= quantile(data$Violent.Crime.Rate, na.rm = T, probs = seq(0, 1, 0.25))[4] ~ 3,
    T ~ 4
  )) %>% 
  filter(yearBuilt != 0)
  
plot_ly(
  data = df,
  x = ~crime,
  y = ~Sale.Price.bid.price,
  frame = ~yearBuilt,
  type = "box"
) %>% 
  layout( 
         xaxis = list(title = "Violent crime rate interval"), 
         yaxis = list(title = "Sale price")) %>% 
  animation_opts(500) %>%
  animation_button(x = 0.05, y = 0.1) %>%
  animation_slider(currentvalue = list(prefix ="Built in ", suffix = "s", font = list(color="black")))
  


