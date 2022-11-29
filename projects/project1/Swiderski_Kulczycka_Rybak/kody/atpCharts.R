library("dplyr")
library("ggplot2")
library(ggpubr)

atpPlayers <- read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_players.csv")

atp_ranking <- read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_rankings_10s.csv") %>% 
  left_join(atpPlayers, by = c("player" = "player_id")) %>%
  filter(height > 160) %>% 
  mutate(age = as.integer(substr(ranking_date, 1, 4)) - as.integer(substr(dob, 1, 4)) + (as.integer(substr(ranking_date, 5, 6)) - as.integer(substr(dob, 5, 6)))/12) %>% 
  mutate(rank = case_when(
    rank <= 10 ~ "top 10",
    TRUE ~ "others"
  ))

age_plot <- ggdensity(atp_ranking, x = "age",
          add = "mean", rug = TRUE,
          color = "rank", fill = "rank",
          palette = c("#00AFBB", "#E7B800"))
age_plot

p <- ggboxplot(atp_ranking, x = "rank", y = "height",
               color = "rank", palette =c("#00AFBB", "#E7B800"),
               shape = "rank")
p
