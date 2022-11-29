setwd("C:/Users/rafp2/OneDrive/Pulpit/RAFAŁ/studia/sem3/TWD/lab/hw4")

library(dplyr)

df <- read.csv("Properties_philly_Kraggle_v2.csv")
filter(df, !is.na(OPA)) %>% 
  filter(PropType != "MultiFamily2To4") -> df
  


plot_ly(
  data = df,
  x = ~Opening.Bid,
  y = ~Sheriff.Cost,
  frame = ~PropType,
  type = "scatter"
) %>% layout(
    title = "Opening bid - Sheriff cost correlation",
    xaxis = list(title = "Opening bid", range = c(0, 40000)),
    yaxis = list(title = "Sheriff cost"),
    updatemenus = list(
      list(
        x = 1, y = 0.9,
        buttons = list(
          list(method = "restyle",
               args = list("marker.color","black"),
               label = "Black"),
          list(method = "restyle",
               args = list("marker.color","green"),
               label = "Green"),
          list(method = "restyle",
               args = list("marker.color","purple"),
               label = "Purple"),
          list(method = "restyle",
               args = list("marker.color","red"),
               label = "Red"))))
) %>% animation_opts(frame = 2500, transition = 700) %>% 
  animation_button(x = 0.00, y = 0.05, label = "Start animation") %>%
  animation_slider(currentvalue = list(visible = F)) %>% 
  config(displayModeBar = FALSE)
