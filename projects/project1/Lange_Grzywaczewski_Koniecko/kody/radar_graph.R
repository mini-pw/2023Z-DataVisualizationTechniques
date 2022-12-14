# F1 Radar graph making
library(ggradar)
library(tidyverse)
library(scales)
library(showtext)
library(reshape2)
library(showtext)

font_add_google("Lobster Two", "lobstertwo")
font_add_google("Roboto", "roboto")
font_add_google("Lato", "lato")
font_add_google("Libre Baskerville", "baskervill")
font_add("formula", "~/.local/share/fonts/Formula1-Regular.ttf")

# Showtext will be automatically invoked when needed
showtext_auto()

f1_res <- read.csv("./proc_data/WeatherAnalysis.csv")
f1_data <- f1_res %>% 
  filter(Redbull != 0 & Mercedes != 0 & Ferrari != 0)

max_air_temp <- f1_data %>% 
  arrange(-AirTempMax) %>% 
  head(1) %>% 
  select(
    c(Redbull, Mercedes, Ferrari)) %>% 
  mutate(Weather = "High Air Temp")

max_wind_speed <- f1_data %>% 
  arrange(-WindSpeedMax) %>% 
  head(1) %>% 
  select(c(Redbull, Mercedes, Ferrari)) %>% 
  mutate(Weather = "Strong Wind")

max_rain <- f1_data %>% 
  arrange(-Rain) %>% 
  head(1) %>%
  select(c(Redbull, Mercedes, Ferrari)) %>% 
  mutate(Weather = "Heavy Rain")

low_pressure <- f1_data %>% 
  arrange(PressureMax) %>% 
  head(1) %>% 
  select(c(Redbull, Mercedes, Ferrari)) %>% 
  mutate(Weather = "Low Pressure")

monaco <- f1_data %>% 
  filter(X == "Monaco Grand Prix") %>% 
  select(c(Redbull, Mercedes, Ferrari)) %>% 
  mutate(Weather = "Changing Weather")

res <- rbind(max_air_temp, max_wind_speed, monaco, max_rain, low_pressure)
melt(res, id.vars = "Weather", measure.vars = c("Redbull", "Mercedes", "Ferrari"),
     variable.name = "Team", value.name = "Score") %>% 
  tidyr::pivot_wider(names_from = Weather, values_from = Score) -> graph_data

graph_data[, names(graph_data) != "Team"] <- graph_data[, names(graph_data) != "Team"] / 44
graph_data

graph_data %>% 
  ggradar(
    font.radar = "roboto",
    grid.label.size = 17,
    gridline.label.offset = -0.1,
    axis.label.size = 10,
    axis.label.offset = 1.13,
    axis.line.colour = "#dfe6e9",
    axis.labels = c(
      "High Temperature",
      "Strong Wind",
      "Changing Weather",
      "Heavy Rain",
      "Low Pressure"
    ),
    group.point.size = 5,
    group.colours = c(
      "#1e5bc6",
      "#6cd3bf",
      "#ed1c24"
    )
  ) + 
  theme(
    
    legend.position = c(1.15, 0.22),  
    legend.justification = c(1, 0),
    legend.text = element_text(size = 26, family = "roboto", color = "#dfe6e9"),
    legend.key = element_rect(fill = NA, color = NA),
    legend.background = element_blank()
  ) + 
  labs(title = "Percentage of points", subtitle = "gathered by teams in differently conditioned races") + 
  theme(
    text = element_text(family = "formula"),
    plot.background = element_rect(fill = "#383F51", color = "#383F51"),
    panel.background = element_rect(fill = "#383F51", color = "#383F51"),
    plot.title.position = "plot", # slightly different from default
    plot.margin = margin(10, 50, 10, 0),
    plot.title = element_text(
      family = "formula",
      size = 50,
      face = "bold", 
      color = "#ffffff"
    ),
    plot.subtitle = element_text(
      family = "formula",
      size = 30,
      color = "#d2dae2",
      margin = margin(0, 0, 30, 0)
    )
  ) -> plt
plt$layers[[1]]$aes_params <- c(plt$layers[[1]]$aes_params, colour = "#ffffff")
plt$layers[[5]]$aes_params <- c(plt$layers[[5]]$aes_params, colour = "#ffffff")
plt$layers[[6]]$aes_params <- c(plt$layers[[6]]$aes_params, colour = "#ffffff")
plt$layers[[11]]$aes_params <- c(plt$layers[[11]]$aes_params, colour = "#ED9B40")
plt$layers[[12]]$aes_params <- c(plt$layers[[12]]$aes_params, colour = "#ED9B40")
plt$layers[[13]]$aes_params <- c(plt$layers[[13]]$aes_params, colour = "#ED9B40")

ggsave(
  filename = "~/Code/F1ProjectTWD/graphs/radar.png",
  plot = plt,
  width = 6,
  height = 5,
  device = "png"
)

pdf("radar.pdf", 
    width = 6,
    height = 5,
    family = "Formula1 Display-Regular")                  # Apply pdf function
plt
dev.off()
