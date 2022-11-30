library(dplyr)
library(tidyr)
library(plotly)


properties <- read.csv("./Properties_philly_Kraggle_v2.csv")

#Making data frame that contains info about number of houses built each year 
#by type


df <- data_frame()

for(i in 1875:2006){
    tmp_df <- properties %>% 
    filter(yearBuilt <= i) %>% 
    group_by(PropType) %>% 
    count() %>% 
    mutate(yearBuilt = i)
    
    df <- rbind(df, tmp_df)
}
df

#Creating animated graph
graph <- plot_ly(data = df, x = ~PropType, y = ~n, frame = ~yearBuilt, type = "bar", showlegend = FALSE) %>% 
  layout(title = "Number of properties built each year by type",
         yaxis = list(title = "Count"),
         xaxis = list(title = "Property type"),
         xaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 5,
           gridcolor = 'ffff'),
         yaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'))

graph

