library(dplyr)
library(readxl)
library(tidyverse)
library(ggplot2)

LegalDrinkingAge <- read_xlsx("C:\\Users\\zareb\\OneDrive\\Desktop\\LegalDrinkingAge.xlsx") %>%
  rename(region = Country) %>%
  rename(age = Age)


map <- map_data("world") %>%
  mutate(region = replace(region, region == "Democratic Republic of the Congo", "Democratic Republic of Congo"))
map <- left_join(map, LegalDrinkingAge, by = "region") %>%
  drop_na(age) %>%
  mutate(age = replace(age, age == "18-21 (or illegal)**", 19)) %>%
  mutate(age = replace(age, age == "18-21 (or illegal)***", 19)) %>%
  mutate(age = replace(age, age == "18-19", 19)) %>%
  mutate(age = replace(age, age == "18-19*", 19))


map <- ggplot(map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = age)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        rect = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_blank()) +
  guides(fill=guide_legend(title="Legal drinking age in years")) +
  ggtitle("Drinking Age \n Across the Globe") + 
  theme(legend.position="bottom") + 
  scale_fill_brewer(palette="Spectral") +
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"),
        legend.title=element_text(color="red", size=14, face="bold.italic"))

map
