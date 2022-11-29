library(dplyr)
library(stringi)
library(ggplot2)

sports <- c("tennis", "basketball", "soccer", "baseball", "olympians")
Age <- c(0,0,0,0,0)

### tenis
# ATP World Tour tennis data
# lista rankingowa mężczyzn na rok 2017
# https://datahub.io/sports-data/atp-world-tour-tennis-data
# rankings_1973-2017
read.csv("dane_projekt1/tenis.csv") -> tenis
str(tenis)
nrow(tenis)

tenis %>%
  na.omit() %>% 
  #filter(week_year > 2010) %>% 
  summarise(age = mean(player_age)) -> res
Age[1] <- res$age
# 22.86754

### koszykówka
# https://www.kaggle.com/datasets/wyattowalsh/basketball
# NBA
read.csv("dane_projekt1/basketball_Player.csv") -> active_players
str(active_players)
nrow(active_players)

active_players %>% 
  filter(is_active == 1) %>% 
  select(id) -> active_players
active_players <- active_players$id

read.csv("dane_projekt1/basketball_Player_Attributes.csv") -> players
str(players)
nrow(players)

month = 11
year = 2022

players %>% 
  select(ID, BIRTHDATE) %>% 
  filter(ID %in% active_players) %>% 
  mutate(BIRTH_YEAR = strftime(BIRTHDATE, "%Y"), BIRTH_MONTH = strftime(BIRTHDATE, "%m")) %>% 
  mutate(AGE = as.numeric(year) - as.numeric(BIRTH_YEAR) - ifelse(BIRTH_MONTH > month, 0, 1)) %>% 
  summarise(age = mean(AGE)) -> res
Age[2] <- res$age
# 27.50674

### piłka nożna
# fifa 22 dataset
# mężczyźni 2022 rok
# https://sports-statistics.com/soccer/fifa-2022-dataset-csvs/
read.csv("dane_projekt1/soccer.csv") -> soccer
str(soccer)
nrow(soccer)

soccer %>% 
  summarise(age = mean(age)) -> res
Age[3] <- res$age
# 25.21082

### baseball
# https://www.seanlahman.com/baseball-archive/statistics/
# People
# ligi amerykańskie
# https://www.seanlahman.com/files/database/readme2021.txt
read.csv("dane_projekt1/baseball.csv") -> baseball
str(baseball)
nrow(baseball)

baseball %>% 
  mutate(year_end = as.numeric(stri_extract_first(finalGame, regex = ".{4}")), year_start = as.numeric(stri_extract_first(debut, regex = ".{4}"))) %>% 
  filter(year_end != "" & year_start != "") %>% 
  mutate(age_end = year_end - birthYear, age_start = year_start - birthYear) %>% 
  mutate(age = (age_start + age_end) / 2) %>% 
  filter(!is.na(age)) %>% 
  summarise(age = mean(age)) -> res
Age[4] <- res$age
# 26.75874

### IO
# https://www.kaggle.com/datasets/heesoo37/120-years-of-olympic-history-athletes-and-results
read.csv("dane_projekt1/IO.csv") -> IO
str(IO)
nrow(IO)
length(unique(IO$ID))

IO %>% 
  group_by(ID) %>% 
  summarise(Age = min(Age)) %>% 
  ungroup() %>% 
  na.omit() %>% 
  summarise(age = mean(Age)) -> res
Age[5] <- res$age
# 24.4

sports
Age

df <- data.frame(sports, age = Age)

# Kolorki
c1 = "#ffde59"
c2 = "#00c2cb"
size1 = 12

# wykres Słupkowy
df %>% 
  mutate(age = round(age, 1)) %>% 
  ggplot(aes(x = sports, y = age)) +
  geom_col(color = c1, fill = c1) +
  geom_text(aes(label = age), vjust = 2, color = "black", size = 6) + 
  theme(
    rect = element_blank(),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    axis.text.x = element_text(size = 12, colour = "black"),
    axis.text.y = element_text(size = 12, colour = "black"),
    plot.title = element_text(size = 30, hjust = 0.5)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Average athletes' age in particular sports",
    x = "sport",
    y = "age[years]"
    ) -> plot

plot

# wymiary 800 na 600
