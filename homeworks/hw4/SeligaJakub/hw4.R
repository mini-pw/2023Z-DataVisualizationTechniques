philly<-read.csv("philly.csv")


library(dplyr)
library(ggplot2)


df<-philly %>% 
  filter_at(vars(Violent.Crime.Rate, Opening.Bid, bedrooms, bathrooms), all_vars(!is.na(.)))


plot_ly(
  data = df, 
  x = ~Violent.Crime.Rate, 
  y = ~Opening.Bid,
  type = "scatter",
  color=~Sheriff.Cost,
  mode = "markers",
  text = paste0("sheriff cost: ", df$Sheriff.Cost, "<br>gas bill: ", df$PGW),
  hoverinfo = 'x+y+text',
  hovertemplate = paste('<br><b>Violent crime rate</b>: %{x}<br><b>Opening bid</b>: %{y} <extra>%{text}</extra>')
  )%>% layout(
    title = "Violent crimes rate vs opening bid price",
    xaxis = list(title = "Violent crimes rate"),
    yaxis = list(title = "Opening bid"),
    updatemenus = list(
      list(
        y = 0.4, x=1.2,
        buttons = list(
          list(method = "update",
               args = list(list("marker.color"= list(~Sheriff.Cost))),
               label = "color: sheriff cost"),
          list(method = "update",
               args = list(list("marker.color" = list(~PGW))),
               label = "color: gas bill")
        ))
    ))
