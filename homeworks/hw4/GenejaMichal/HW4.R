library(ggplot2)
library(plotly)
library(dplyr)

HW4_data <- read.csv("HW4_data.csv")
HW4_data <- select(HW4_data,
                   Opening.Bid,
                   Postal.Code,
                   Sheriff.Cost,
                   Advertising,
                   Water,
                   Avg.Walk.Transit.score,
                   Violent.Crime.Rate,
                   School.Score,
                   yearBuilt,
                   finished...SqFt.,
                   bathrooms,
                   bedrooms) %>%
  na.omit()
HW4_data <- filter(HW4_data, HW4_data$yearBuilt>1000)
HW4_data <- mutate(HW4_data,'ZIP_code' = as.factor(HW4_data$Postal.Code%/%10))
  
plot_ly(
  x=HW4_data$yearBuilt,
  y=HW4_data$Opening.Bid,
  color = HW4_data$ZIP_code,
  colors = c("red","orange","green","purple","cyan","blue"),
  type = "scatter",
  mode = 'markers',
  text = paste0("Build in ",HW4_data$yearBuilt,
                "<br>Initial bid: $",HW4_data$Opening.Bid,
                "<br>Postal code: ",HW4_data$Postal.Code),
  hoverinfo = 'text'
  ) %>% 
  layout(
    legend = list(
      title = list(
        text = "Zip Code prefixes"
      )
    )
  )
