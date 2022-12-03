library(plotly)
library(dplyr)

df <- read.csv("Philadelphia.csv")

df <- na.omit(df)
df$Sale.Price.bid.price <- as.numeric(gsub('[$,]', '', df$Sale.Price.bid.price))

price_min <- min(df$Sale.Price.bid.price)
price_max <- max(df$Sale.Price.bid.price)

l <- 1:nrow(df)
number <- nrow(df) / 5

df <- df %>% 
  select(Sale.Price.bid.price, Sheriff.Cost, finished...SqFt., Violent.Crime.Rate, School.Score, Avg.Walk.Transit.score) %>% 
  arrange(Sale.Price.bid.price) %>% 
  mutate(n = l) %>% 
  mutate(price = if_else(n > 4*number, 5, if_else(n > 3*number, 4, if_else(n > 2*number, 3, if_else(n > number, 2, 1)))))

df %>% 
  group_by(price) %>% 
  summarise(count = n())

fig <- plot_ly(data = df, y = ~Sheriff.Cost, frame = ~price, type = "violin")
  

fig <- fig %>% 
  layout(
    title = "", 
    xaxis = list(title = ''),
    yaxis = list(title = 'number'),
    
    updatemenus = list(
      list(
        x = 1, y = 1,
        buttons = list(
          list(method = "restyle", args = list("y", list(df$Sheriff.Cost)),  label = "Sheriff.Cost"),
          list(method = "restyle", args = list("y", list(df$finished...SqFt.)),  label = "finished...SqFt.")
        ))
    )
  )

fig %>% 
  animation_button(visible=FALSE) %>% 
  animation_slider(currentvalue = list(prefix = paste("PRICE BIN: "))) -> fig

fig

