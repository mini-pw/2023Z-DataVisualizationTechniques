library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)


getwd()
df<-read.csv("Properties_philly_Kraggle_v2.csv")

# we will clean our data
df <- df[!is.na(df$Violent.Crime.Rate),]
max(df$Violent.Crime.Rate) #1.72
CrimeRate <- round(df$Violent.Crime.Rate / 1.72 * 100)



#set price as numeric instead of char 
df$Sale.Price.bid.price <- as.numeric(gsub('[$,]', '', df$Sale.Price.bid.price))

#first plot shows all the houses, their prices and crime rates 
fig <- plot_ly(df,
               x = CrimeRate, 
               y=~Sale.Price.bid.price,
               type = "scatter",
               color = ~PropType,
               text = ~paste("Price: ", Sale.Price.bid.price,'<br>Type:', PropType, '$<br>Crime Rate:', CrimeRate,'%',"<br>Cheriff cost", Sheriff.Cost,"$"),
               hoverinfo = 'text'
)


fig%>%
  layout(title = "The price of houses depending on the crime rate in a given area",
         yaxis = list(
           title = "Sale price"),
         xaxis = list(
           title = "Percent of crime rate compared to other districts")
  )

#When you hover your mouse over the legend, individual house types will show up. 
#Also after hovering over the plot you will be able to see the exact values. 

#summing up
#we can see that the price of house is particularly connected to the crime rate in the district, what is not a surprise. 
#there are much more expensive houses in richer areas which are safer and  where sheriff cost is higher than in the poor ones. 



