#map
library(dplyr)
library(ggplot2)
library(tidyverse)
library(geojsonio)
library(broom)
library(RColorBrewer)
library(sf)
library(terra)
library(spData)
library(spDataLarge)
library(spDataLarge)
library(choroplethr)
library(choroplethrMaps)

# Mapa z zaznaczoną liczbą złotych medali zdobytych przez kraj - kartogram

df <- read.csv("athlete_events.csv")


df %>% 
  select(ID, Team, Medal) %>% 
  filter(!is.na(Medal)) %>% 
  mutate(medal =1) 


df %>% 
  select(ID, Team, Medal) %>% 
  filter(!is.na(Medal)) %>% 
  group_by(Team) %>% 
  summarise(suma_medali = sum())


df %>% 
  select(ID, Team, Medal) %>% 
  filter(!is.na(Medal)) %>% 
  mutate(medal_liczba =1) %>% 
  group_by(Team) %>% 
  summarise(suma_medali = sum(medal_liczba)) %>% 
  arrange(-suma_medali)


df_medals <-
  df %>% 
  filter(Season == "Summer") %>% 
  select(ID, Team, Medal) %>% 
  filter(!is.na(Medal)) %>% 
  mutate(rodzaj_medalu = case_when(Medal == "Gold" ~1,
                                   Medal == "Silver" ~2,
                                   Medal == "Bronze"~3,
                                   TRUE ~ 4)) %>% 
  group_by(Team, rodzaj_medalu) %>% 
  summarise(liczba_medali = n()) %>% 
  arrange(-liczba_medali)

#tak naprawde mamy juz cale dane i wystarczy to przeniesc na mape swiata 
#-> kartogram :)


df_zlote_medale <-
  df_medals %>% 
  filter(rodzaj_medalu == 1) %>% 
  select(Team, liczba_medali) %>% 
  filter(liczba_medali > 2)


# proponuje wziac dane od 1991 roku? -> co z Soviet Union i Rosja? West Germany, East?



# =============  reczne zmiany  ==========

#0. usuwamy jak maja liczbe 
warunek <- grepl("[0-9]", df_zlote_medale$Team)
df_zlote_medale <- df_zlote_medale[!warunek,]

#1. zmiana usa
df_zlote_medale[1,1] <- "USA"
df_zlote_medale

#2. ZSRR + rosja? 12
df_zlote_medale[12,2] <- 294+ 830
df_zlote_medale <- df_zlote_medale[-c(2),]

# 3.Germany - East,West? 2,7,19
df_zlote_medale[2,2] <- 564+337+144
df_zlote_medale <- df_zlote_medale[-c(7,19),]

df_zlote_medale <- df_zlote_medale %>% arrange(-liczba_medali)

#4. Unified team -> tez do zsrr 2,27
df_zlote_medale[2,2] <-1124+92
df_zlote_medale <- df_zlote_medale[-c(27),]


#5.Great Britain? -> UK
df_zlote_medale[5,1] <- "UK"

#6. Czechoslovakia?

# 7. Portugalia? -> 4 medale xD






#hehe no to niezla rozbieznosc z wikipedia danych XD

# musimy zrobic liczbe medali dyskretne
# G1: >= 1000 medali
# G2: 400,1000
# G3: 250,400
# G4: 100,250
# G5: 50,100
# G6: 20,50
# G7: 0,20


#WERSJA 1
df_zlote_medale <-
  df_zlote_medale %>% 
  mutate(grupa_n_med = case_when(liczba_medali >= 1000 ~ "1",
                                 liczba_medali >= 400 ~ "2",
                                 liczba_medali >= 250 ~ "3",
                                 liczba_medali >= 100 ~ "4",
                                 liczba_medali >= 50 ~ "5",
                                 liczba_medali >= 20 ~ "6",
                                 liczba_medali >= 1 ~ "7",
                                 TRUE ~ "8"))



mapdata  <- map_data("world")
mapdata <-
  mapdata %>% 
  filter(region != "Antarctica")


joined_map_medals <- left_join(mapdata, df_zlote_medale, 
                               by = c("region" = "Team"))

joined_map_medals$grupa_n_med[is.na(joined_map_medals$grupa_n_med)] <- "8"


kolorki <- brewer.pal(7, "Purples")

kolorki2guard <- c("1" = "#58006E",
                   "2" = "#710174",
                   "3" = "#8F017A",
                   "4" = "#B40D83",
                   "5" = "#C61C8B",
                   "6" = "#E24198",
                   "7" = "#DD6FB8",
                   "8" = "#FEEBE7")

kolorki_zlote <- c("1" = "#6E4700",
                "2" = "#8E5C00",
                "3" = "#B57500",
                "4" = "#CC8500",
                "5" = "#F59F00",
                "6" = "#FFB429",
                "7" = "#FDC864",
                "8" = "#F8DBA5")

napis_tyt <- "Total number of gold medals at summer Olympics"

labelki = c(" ≥ 1000","[400,1000)","[250,400)","[100,250)","[50,100)","[20,50)","[1,20)", "0")

map1 <- 
  joined_map_medals %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill=grupa_n_med), color = "white") +
  theme_void() + 
  theme(plot.title = element_text(size = 22, 
                                  hjust = 0.5, 
                                  color = "#4e4d47",
                                  face = "bold"),
        legend.position = c(0.1,0.3),
        legend.title = element_text(face = "bold")) +
  scale_fill_manual(values =kolorki_zlote, 
                    na.value = "#FEEBE7", 
                    labels = labelki) + 
  labs(title = "", 
       fill = "Number of gold medals") +
  guides(color= guide_legend("tytl"))


map1


ggsave(
  filename = "tr_mapka2.png",
  bg = "transparent",
  width =9,
  height = 4,
  limitsize = FALSE
)


?ggsave
add#https://coolors.co/58006e-710174-8f017a-b40d83-c61c8b

# zgap od tego Mateusza albo z tego guardiana -> tutaj ta mapka jest za dokladna, 
#ogar legend i te kolorki fajne barwy

# a co z grenlandia?

#  theme(panel.grid.major = element_blank(), 
#panel.grid.minor = element_blank(),
#panel.background = element_rect(colour = "black", size=1))

