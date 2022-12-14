library(dplyr)
library(plotly)

data <- na.omit(read.csv("data.csv"))[, c('Violent.Crime.Rate', 'Rent.Estimate', 'PropType')]

estRent <- data %>% 
  select(c('Violent.Crime.Rate', 'Rent.Estimate')) %>% 
  mutate(Rent.Estimate = as.numeric(gsub(",", "", Rent.Estimate)))

min_rent <- min(estRent$Rent.Estimate)
max_rent <- max(estRent$Rent.Estimate)
ranges <- c(min_rent, 1500, 2000, max_rent)
dfs <- split(estRent,cut(estRent$Rent.Estimate,ranges))

fig <- plot_ly(type = "box") %>% 
  layout(title = "\nRelation between estimated rent and\n violent crime in Philadelphia", 
         xaxis = list(title = "Estimated rent in USD"), 
         yaxis = list(title = "Violent crime rate"))

for (i in 1:length(dfs)) {
  fig <- fig %>% add_trace(data = dfs[[i]], y = ~Violent.Crime.Rate, name=paste0(ranges[i], " - ", ranges[i+1]))
}

fig
