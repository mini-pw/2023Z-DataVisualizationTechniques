library(plotly)
library(dplyr)


df <- read.csv("Dane.csv")
colnames(df)

Postcodes <- df %>% 
  group_by(Postal.Code) %>% 
  summarise(n()) %>% 
  filter(`n()` > 10) %>% 
  na.omit() 

Rents <- df %>% 
  group_by(Rent.Estimate) %>% 
  summarise(n()) %>% 
  filter(`n()` > 15) %>% 
  na.omit() 




data <- df %>% 
  filter(Rent.Estimate %in% Rents$Rent.Estimate) %>% 
  filter(PropType != "MultiFamily2To4") %>% 
  select(PropType, Violent.Crime.Rate, Rent.Estimate)
  
text <- list(
  family = "sans serif",
  size = 14,
  color = toRGB("grey50"))

plot_ly(
  data = data,
  y = ~Violent.Crime.Rate,
  x = ~Rent.Estimate,
  type = "box",
  color = ~PropType,
  colors = c("red", "blue", "green"),
  hoverinfo = "text",
  hovertext = paste("Crime Rate:", data$Violent.Crime.Rate,
                      "<br>Est. Rent:", data$Rent.Estimate)) %>% 
  layout(
    plot_bgcolor = "#e5ecf6",
    legend = list(title=list(text='<b> Household type: </b>')),
    title = list(text = "<b>Rent in different crime rates </b>",y = 0.995, font = list(size = 25)),
    xaxis = list(title = "Estimated Rent [$]",titlefont = list(size = 20)),
    yaxis = list(title = "Violent Crime Rate", titlefont = list(size = 20)),
    updatemenus = list(
      list(
        x = 1.15, y = 0.5,
        buttons = list(
          list(method = "restyle",
               args = list("type", "box"),
               label = "Boxplot"),
          list(method = "restyle",
               args = list("type", "violin"),
               label = "Violinplot")
          
        ))
    )) 

