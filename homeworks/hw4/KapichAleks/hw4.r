library(plotly)
library(ggplot2)
library(dplyr)
library(stringi)

estate <- read.csv("./Properties_philly_Kraggle_v2.csv") %>% 
  select(Address,
         Sale.Price = Sale.Price.bid.price,
         Violent.Crime.Rate) %>% 
  na.omit() %>% 
  mutate(Crime.Rate.Quartile = ntile(Violent.Crime.Rate, 4),
         Sale.Price=as.numeric(gsub(",","", stri_sub(Sale.Price,2))))


plot_ly(
  data = estate,
  x = ~Crime.Rate.Quartile,
  y = ~Sale.Price,
  type = "box") %>% 
  layout(
    title = "Sale price distribution in relation to crime rate",
    xaxis = list(title = "Crime rate quartile"),
    yaxis = list(title="Sale price",
                 range = c(0, 360000)),
    updatemenus = list(
      list(
        x = 1, y = 1,
        buttons = list(
          list(method = "restyle",
               args = list("boxpoints", "none"),
               label = "Boxpoints OFF"),
          list(method = "restyle",
               args = list("boxpoints", "all"),
               label = "Boxpoints ON")
        ))
    ))