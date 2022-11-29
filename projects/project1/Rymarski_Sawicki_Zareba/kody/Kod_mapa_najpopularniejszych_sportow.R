install.packages("ggplot2")
install.packages("tidyverse")

library(ggplot2)
library(tidyverse)
library(dplyr)

windowsFonts(hel=windowsFont("Sans"))

SportByCountry <- read.csv("C:\\Users\\zareb\\OneDrive\\Desktop\\Studies\\3 semestr\\R\\projekt1\\MostPopularSport.csv")
SportByCountry <- SportByCountry %>%
  rename(region = country)
#dataframe
mapdata <- map_data("world") %>%
  mutate(region = replace(region, region == "USA", "United States")) %>%
  mutate(region = replace(region, region == "UK", "United Kingdom")) %>%
  mutate(region = replace(region, region == "Democratic Republic of the Congo", "DR Congo"))
mapdata <- right_join(mapdata, SportByCountry, by = "region")
mapdata <- mapdata %>%
  mutate(babbel2021 = ifelse(babbel2021  == "", NA, babbel2021 )) 
mapdata <- mapdata %>%
  drop_na(babbel2021)
#paleta kolorow do oznaczen
colorPalette <- c("#00C2CB", "#C4C4C4", "#3F63B5", 
                  "#900C3F", "#fffb00", "#ff00af", "#B98D74", 
                  "#015D57", "#B54756", "#FF9001", "#FF4200", 
                  "#00fe00", "#FFDE59")

#dodatkowe, do zmiany legendy
mapaUp <- mapdata %>%
  select(region, babbel2021) %>%
  distinct() %>%
  select(babbel2021) %>%
  count(babbel2021) %>%
  rename(sport = babbel2021) %>%
  rename(HowManyCountries = n) %>%
  arrange(desc(HowManyCountries))

#wektor nazw
StringVectorSports <- collect(mapaUp)$sport

#mapa
mapdata$babbel2021 <- factor(mapdata$babbel2021, levels = StringVectorSports)
map <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = babbel2021)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        rect = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size=30, hjust = 1.1, face = "plain", family = "hel"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        plot.background = element_rect("white")) +
  scale_fill_manual(values = colorPalette) +
  guides(fill=guide_legend(title="Sport by color")) +
  ggtitle("Most popular sport by country")
map

#zapisanie
ggsave("mapka.png",
       width = 11,
       height = 6,
       dpi = 1000)
