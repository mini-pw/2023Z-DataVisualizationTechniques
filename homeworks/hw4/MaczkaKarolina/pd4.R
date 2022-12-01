library("dplyr")
library("ggplot2")
library("plotly")

df<-read.csv("Properties_philly_Kraggle_v2.csv")
df$Sale.Price.bid.price <- as.numeric(gsub('[$,]', '', df$Sale.Price.bid.price))

fig <- plot_ly(df, x = ~School.Score, y=~Sale.Price.bid.price,type = "scatter", marker=list(color="black"))

fig <- fig %>% layout(
  title = "Cena w zależności od dystryktu szkolnego",
  xaxis = list(type=numeric(), title="School score"),
  yaxis = list(title = "Cena"),
  updatemenus = list(
    list(
      y = 0.8,
      buttons = list(
        list(method = "restyle",
             args = list("marker.color","black"),
             label = "Black"),
        list(method = "restyle",
             args = list("marker.color","blue"),
             label = "Blue"),
        list(method = "restyle",
             args = list("marker.color","green"),
             label = "Green"))),
    list(
      y = 0.7,
      buttons = list(
        list(method = "restyle",
             args = list(
               list(type = list("scatter"), mode = list("markers"))),
             label = "Scatter Plot"),
        
        list(method = "restyle",
             args = list(
               list(type = list("histogram"))),
             label = "Histogram")
      )
    )
  )
  )
  
fig

