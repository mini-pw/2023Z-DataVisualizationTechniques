library(plotly)
library(dplyr)
library(sysfonts)
font_add_google("Roboto")

df <- read.csv("./resources/players_in_chess.csv")

filtered <- df %>%
  group_by(country) %>%
  summarise(
    average_rating = mean(rating, na.rm = TRUE),
    max_rating = max(rating, na.rm = TRUE),
    number_of_fide_players = n()
  )

all_chess <- filtered %>%
  left_join(df[c("name", "country", "rating")], by = c("country" = "country", "max_rating" = "rating"))

g <- list(
  showframe = FALSE,
  showcoastlines = TRUE,
  projection = list(type = 'Mercator'),
  bgcolor = "transparent"
)

plot_geo(all_chess) %>%
  add_trace(
    z = ~number_of_fide_players,
    color = ~number_of_fide_players,
    colors = "Oranges",
    locations = ~country
  ) %>%
  colorbar(title = 'Number of FIDE players') %>%
  layout(
    title = list(
      y = 0.95,
      text = "Number of FIDE players per this country",
      font = list(
        family = "Roboto",
        size = 30
      )
    ),
    geo = g
  )


  all_chess %>%
  plot_geo(
    type = "choropleth",
    z = ~number_of_fide_players,
    locations = ~country,
    colorscale = "inferno"
  )
