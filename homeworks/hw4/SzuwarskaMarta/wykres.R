library(plotly)
library(dplyr)

df <- read.csv("Properties_philly_Kraggle_v2.csv")

Sys.setlocale(category = "LC_TIME", locale="en_GB.UTF-8")

df %>%
  na.omit() %>% 
  mutate(saleYear = substr(Sale.Date,(nchar(Sale.Date)+1)-4,nchar(Sale.Date))) %>% 
  group_by(Postal.Code) %>% 
  plot_ly(x = ~Avg.Walk.Transit.score,
          y = ~School.Score,
          symbol = ~PropType,
          frame = ~Postal.Code,
          type = "scatter",
          mode = "markers",
          text = ~paste("Average Walk Transit Score: ",Avg.Walk.Transit.score,"<br>School score: ",School.Score),
          hoverinfo = "text") %>% 
  animation_slider(
    currentvalue = list(prefix = "Postal code: ", font = list(color="purple"))
  ) %>% 
  layout(title = "Plot of comparison of average walk transit score and school score",
         xaxis = list(title = "Average walk transit score"), 
         yaxis = list(title = "School score"),
         legend = list(x = 1,
                       y = 0.8,
                       title = list(text = "Property type:"))
         )
  
