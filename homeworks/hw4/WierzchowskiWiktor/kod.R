getwd()
options(stringsAsFactors = FALSE)

df <- read.csv("archive/Properties_philly_Kraggle_v2.csv")

library(plotly)
library(dplyr)

head(df)

df <- df %>%
  filter(yearBuilt > 1500) %>%
  mutate(bucket = ntile(finished...SqFt., 10))

my_min <- df %>%
  select(bucket, finished...SqFt.) %>%
  group_by(bucket) %>%
  summarise(min(finished...SqFt.))

my_max <- df %>%
  select(bucket, finished...SqFt.) %>%
  group_by(bucket) %>%
  summarise(max(finished...SqFt.))

my_range <- full_join(my_max, my_min, by = "bucket")
my_range$`min(finished...SqFt.)` <- as.character(my_range$`min(finished...SqFt.)`)
my_range$`max(finished...SqFt.)` <- as.character(my_range$`max(finished...SqFt.)`)

my_range <- my_range %>%
  mutate(range = paste(`min(finished...SqFt.)`, `max(finished...SqFt.)`, sep="-")) %>%
  select(range, bucket)

df <- full_join(df, my_range, by="bucket")


test <- df %>%
  plot_ly(
    x = ~yearBuilt,
    y = ~Opening.Bid,
    split = ~PropType,
    frame = ~range,
    type = 'scatter',
    mode = 'markers'
) %>%
  layout(
    title = "Zale¿noœæ ceny wystawowej domostwa od roku budowy 
    w ró¿nych przedzia³ach powierzchniowych.",
    xaxis = list(
      title = "Rok budowy",
      zeroline = F
    ),
    yaxis = list(
      title = "Cena wystawowa",
      zeroline = F
    )
  ) %>%
  animation_opts(
    frame = 1000, 
    transition = 500, 
    redraw = FALSE
  ) %>%
  animation_slider(
    
  ) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )
test

