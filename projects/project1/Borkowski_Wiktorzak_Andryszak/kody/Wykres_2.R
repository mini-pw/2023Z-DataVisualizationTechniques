library(dplyr)
library(ggplot2)
library(forcats)
library(stringi)

gole <- read.csv("goals.csv")
turnieje <- read.csv("tournaments.csv")
wystepy_zespolow <- read.csv("team_appearances.csv")

wygrani <- turnieje %>% mutate(winner = case_when(
  winner == "West Germany" ~ "Germany",
  TRUE ~ winner
)) %>% select(year, winner) %>% transmute(para_rok_wygrany = paste(as.character(year),"",as.character(winner)))

df1 <- wystepy_zespolow %>% 
  mutate(year = stri_sub(tournament_name, 0, 4)) %>% 
  mutate(team_name = case_when(
    team_name == "West Germany" ~ "Germany",
    TRUE ~ team_name
  )) %>%
  group_by(year, team_name) %>% 
  summarise(matches = n(), goals_scored = sum(goals_for), .groups = "drop_last") %>%
  mutate(para = paste(as.character(year), "", as.character(team_name))) %>% 
  filter(para %in% wygrani$para_rok_wygrany)

df2 <- gole %>%
  mutate(year = stri_sub(tournament_name, 0, 4)) %>% 
  group_by(year, team_name) %>% 
  summarise(goals = n()) %>% 
  mutate(goals = median(goals))

inner_join(df1, df2, by="year") %>% 
  ggplot(aes(x=year)) +
  geom_point(aes(y = goals_scored, col = "goals_scored"), size=13) +
  geom_point(aes(y = goals, col = "goals"), size=13) +
  geom_text(aes(label=goals_scored, y = goals_scored), color="white", size=4) +
  geom_text(aes(label=goals, y = goals), color="white", size=4) +
  labs(
    x = "Rok mistrzostw",
    y = "Strzelone bramki") +
  scale_color_manual(name= "",
                     labels = c("Zwycięzcy", "Mediana"),
                     values = c("goals_scored"="#00326f", "goals"="grey")) +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(size = 18))
