library("plotly")
library("ggplot2")
library("dplyr")

#Zapisujemy plik
proPhi <- read.csv("C:\\Users\\zareb\\OneDrive\\Desktop\\Properties_philly_Kraggle_v2.csv")

#Tworzymy nasz wykres
fig <- plot_ly(
  data = proPhi,
  x = ~finished...SqFt.,
  frame = ~PropType,
  type = "box"
  
) %>%
  layout(title = "Finished square footage depending on the type of property",
         xaxis = list(title = "finished square footage"),
         updatemenus = list(
           list(
             x = 1, y = 0.9,
             buttons = list(
               list(method = "restyle",
                    args = list("marker.color","black"),
                    label = "Black"),
               list(method = "restyle",
                    args = list("marker.color","red"),
                    label = "Red"),
               list(method = "restyle",
                    args = list("marker.color","blue"),
                    label = "Blue"))),
           list(
             x = 1, y = 1,
             buttons = list(
               list(method = "restyle",
                    args = list("type", "box"),
                    label = "Boxplot"),
               list(method = "restyle",
                    args = list("type", "violin"),
                    label = "Violinplot"))
           )
         )
  ) %>% 
  animation_slider(currentvalue = list(prefix = "Property type"))

#Odpalamy wykres
fig
