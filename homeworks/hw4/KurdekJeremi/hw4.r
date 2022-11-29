library(plotly)
library(dplyr)
library(readr)

df <- read.csv("Properties_philly_Kraggle_v2.csv")

df$decade <- floor(df$yearBuilt / 10) * 10

df <- df %>%
  filter(yearBuilt > 0) %>%
  mutate(decade = factor(decade)) %>%
  mutate(price = parse_number(Sale.Price.bid.price))

fig <- plot_ly(
  data = df,
  x = ~price,
  y = ~finished...SqFt.,
  frame = ~decade,
  type = "scatter",
  mode = "markers"
) %>%
layout(
  title = "Price vs. Area per Decade",
  xaxis = list(title = "Price ($)"),
  yaxis = list(title = "Area (sqft)")
)

fig %>%
  animation_opts(2000) %>%
  animation_button(x = 0.05, y = 0.05, label = "Animate") %>%
  animation_slider(currentvalue = list(prefix = "Decade built: ", font = list(size=15, color="black")))


