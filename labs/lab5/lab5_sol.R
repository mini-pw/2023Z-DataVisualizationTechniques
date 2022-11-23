###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 5            ###
###########################################

library(ggplot2)
library(dplyr)
library(SmarterPoland)

## Zadanie 1
# Z zamieszczonego pliku .pdf w folderze lab5 należy rozwiązać jedno z dwóch zadań. 
# Dane potrzbne do odtowrzenia wizualizacji wczytujemy następująco:

df <- read.csv(file = "https://raw.githubusercontent.com/R-Ladies-Warsaw/PoweR/master/Cz%C4%99%C5%9B%C4%87%202%20-%20Formatowanie%20danych/R/data/ranking.csv", 
               encoding = "UTF-8")


p1 <- ggplot(df, aes(x = Genre, y = Score)) + 
  geom_boxplot(outlier.color = "#731ea3") +
  scale_x_discrete(guide = guide_axis(n = 2)) +
  scale_y_continuous(limits = c(0, 10), expand = c(0,0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6)) +
  labs(x = "",
       y = "Ocena",
       title = "Rozkład ocen dla poszczególnych gatunków gier.")
p1


df2 <- df %>%
  group_by(Publisher) %>%  
  summarise(n = n()) %>%  
  filter(n > 100) %>% 
  left_join(df) %>% 
  filter(Year_release > 2013, Year_release < 2016) %>% 
  group_by(Publisher, Year_release) %>% 
  summarise(n1 = n()) %>%
  ungroup() %>%
  mutate(Publisher = stringr::str_trunc(Publisher, 16, side="right"))

p2 <- ggplot(df2, aes(x = reorder(Publisher, n1), 
                      y = n1, 
                      fill = factor(Year_release))) + 
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("navyblue", "darkgreen")) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.25)) +
  coord_flip() + 
  labs(y = "Liczba gier",
       x = "Wydawnictwo",
       fill = "Rok",
       title = "Liczba gier wydanych w 2014 i 2015 przez wydawnictwa,",
       subtitle = "które w latach 1970-2020 wydały ponad 100 gier.")
p2


## patchwork

library(patchwork)

p2 + p1

p1 / p2

(p1 + p2) / 
  ggplot(df) + 
  geom_bar(aes(Year_release)) + 
  labs(title = "Liczba gier wydanych w poszczególnych latach",
       y = NULL, x = "Rok") 

p3 <- ggplot(df) + 
  geom_bar(aes(Genre)) + 
  scale_x_discrete(guide = guide_axis(n = 2)) +
  labs(title = "Liczba gier wydanych w poszczególnych gatunkach",
       y = NULL, x = "Gatunek")

(p1 / p3) & theme_minimal()

?patchwork::plot_layout
(p1 / p3) +
  patchwork::plot_layout(heights = c(3, 1))

?patchwork::plot_annotation
(p1 / p3 + labs(y = "Liczba")) +
  patchwork::plot_annotation(title = "Raport",
                             subtitle = "Rozkład ocen i liczba gier wydanych w poszczególnych gatunkach",
                             caption = Sys.time()) &
  labs(title = NULL) &
  theme_bw()


## ggrepel

library(ggrepel)

p_points <-  ggplot(countries, aes(x = birth.rate, y = death.rate, label = country)) +
  geom_point() 

p_points + geom_text()

?ggrepel::geom_text_repel
p_points + geom_text_repel() # Warning!

p_points + geom_text_repel(max.overlaps = 20,
                           max.time = 2, 
                           max.iter = 20000,
                           label.size = 0.1,
                           size = 2,
                           color = ifelse(countries$continent == "Africa", "red", "black"))

?ggrepel::geom_label_repel
ggplot(countries, aes(x = birth.rate, y = death.rate, label = country)) +
  geom_point() + 
  geom_label_repel()

# Więcej: https://ggrepel.slowkow.com/articles/examples.html


## Zadanie 2
# Narysuj wykres punktowy zależności między wskaźnikiem urodzeń a wskaźnikiem śmierci 
# oraz podpisz punkty o najniższym i najwyższym wskaźniku śmiertelności (nazwą kraju).

countries_labeled <- countries %>% 
  mutate(country_min_max = ifelse(death.rate %in% c(min(death.rate), max(death.rate)), country, ""))

p <- ggplot(countries_labeled, aes(x = birth.rate, y = death.rate, label = country_min_max)) + 
  geom_point() +
  geom_text_repel(min.segment.length = 0)

p


## Zadanie 3 - stworzyć wykres gęstości brzegowych:
# a) wykres punktowy dwóch wskaźników + kolor
# b) dodać po lewej rozkład zmiennej death.rate
# c) dodać na dole rozkład zmiennej birth.rate

main_plot <- ggplot(data = countries, aes(x = birth.rate, y = death.rate, color = continent)) +
  geom_point()

density_death <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  coord_flip() +
  scale_y_reverse() +
  theme(legend.position = "none")


density_birth <- ggplot(data = na.omit(countries), aes(x = birth.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  scale_y_reverse() +
  theme(legend.position = "none")

density_death + main_plot + plot_spacer() + density_birth +
  plot_layout(ncol = 2, heights = c(0.7, 0.3), widths = c(0.3, 0.7))


## mapy

require(maps)
?map_data

# maps::county(), maps::france(), maps::italy(), maps::nz(), maps::state(), maps::usa(), maps::world(), maps::world2()

state <- map_data("state")

random_var <- data.frame("region" = unique(state$region), "rand" = rnorm(length(unique(state$region)), mean = 1, sd = 5))

us <- state %>% 
  left_join(random_var) %>% 
  ggplot(aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = rand))

?coord_map()

us + coord_map()

us + coord_map("gilbert")

us + coord_map("conic", lat0 = 30)

us + coord_map("bonne", lat0 = 50)

# Więcej: https://ggplot2.tidyverse.org/reference/coord_map.html

## Zadanie 4
# Wykorzystując dane countries narysuj mapę świata i zaznacz na niej wskaźnik urodzeń. 

country <- map_data("world")

country %>% 
  left_join(countries, by = c("region" = "country")) %>% 
  ggplot(aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = birth.rate)) 


## ggpubr

devtools::install_github("kassambara/ggpubr")

library(ggpubr)
set.seed(1234)
wdata = data.frame(
  sex = factor(rep(c("F", "M"), each=200)),
  weight = c(rnorm(200, 55), rnorm(200, 58)))
head(wdata, 4)

ggdensity(wdata, x = "weight",
          add = "mean", rug = TRUE,
          color = "sex", fill = "sex",
          palette = c("#00AFBB", "#E7B800"))

gghistogram(wdata, x = "weight",
            add = "mean", rug = TRUE,
            color = "sex", fill = "sex",
            palette = c("#00AFBB", "#E7B800"))

# Więcej: https://github.com/kassambara/ggpubr/
