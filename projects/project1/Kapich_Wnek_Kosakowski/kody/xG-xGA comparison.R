install.packages("devtools")
devtools::install_github("JaseZiv/worldfootballR",force=TRUE)
install.packages("ggimage")
install.packages('janitor')
install.packages('rsvg')
install.packages('ggrepel')
install.packages('hrbrthemes')
library(dplyr)
library(worldfootballR)
library(ggplot2)
library(ggrepel)
library(hrbrthemes)
library(ggimage)
library(janitor)
#L1 2022/2023 DANE
L1_22_urls <- fb_match_urls(country="FRA", gender="M", season_end_year = 2023, tier="1st")
L1_22_stats_2 <- fb_match_summary(match_url=L1_22_urls)
L1_22_stats<-L1_22_stats_2

#WYSELEKCJONOWANIE POTRZEBNYCH KOLUMN Z RAMKI DANYCH
janitor::clean_names(L1_22_stats) %>%
  filter(team=="Lorient") %>%
  select(match_date, matchweek, home_team, home_x_g, home_score, away_team, away_x_g, away_score, team, home_away)->L1_22_stats

#EDYCJA RAMKI DANYCH DO POTRZEBNEJ POSTACI
L1_22_stats$xG<-ifelse(L1_22_stats$team == L1_22_stats$home_team, L1_22_stats$home_x_g, L1_22_stats$away_x_g)

L1_22_stats$xGA<-ifelse(L1_22_stats$team == L1_22_stats$home_team, -L1_22_stats$away_x_g, -L1_22_stats$home_x_g)

L1_22_stats$result<-ifelse(L1_22_stats$team == L1_22_stats$home_team,
                           ifelse(L1_22_stats$home_score > L1_22_stats$away_score,"W",
                                  ifelse(L1_22_stats$home_score < L1_22_stats$away_score,"L","D")),
                           ifelse(L1_22_stats$home_score < L1_22_stats$away_score,"W",
                                  ifelse(L1_22_stats$home_score > L1_22_stats$away_score,"L","D")))

L1_22_stats$opponent <- ifelse(L1_22_stats$team == L1_22_stats$home_team,L1_22_stats$away_team, L1_22_stats$home_team)
L1_22_stats<-L1_22_stats[,c(11:14)]
L1_22_stats<-unique(L1_22_stats)
L1_22_stats<-L1_22_stats[-c(13,14),]
L1_22_stats <- L1_22_stats[]
L1_22_stats <- L1_22_stats[1:12, ]
L1_22_stats$gameweek<-c(1:12)
L1_22_stats$team<-"Lorient"


L1_22_stats <- L1_22_stats %>%
  mutate(opponent_logo = case_when(
    opponent == "Rennes" ~ "https://upload.wikimedia.org/wikipedia/en/9/9e/Stade_Rennais_FC.svg",
    opponent == "Toulouse" ~ "https://upload.wikimedia.org/wikipedia/en/6/63/Toulouse_FC_2018_logo.svg",
    opponent == "Clermont Foot" ~ "https://upload.wikimedia.org/wikipedia/en/5/52/Clermont_Foot_logo.svg",
    opponent == "Lens" ~ "https://upload.wikimedia.org/wikipedia/en/c/cc/RC_Lens_logo.svg",
    opponent == "Ajaccio" ~ "https://upload.wikimedia.org/wikipedia/en/1/1f/AC_Ajaccio_logo.svg",
    opponent == "Lyon" ~ "https://upload.wikimedia.org/wikipedia/de/f/f1/Olympique_Lyon.svg",
    opponent == "Nantes" ~ "https://upload.wikimedia.org/wikipedia/commons/4/45/Logo_FC_Nantes_%28avec_fond%29_-_2019.svg",
    opponent == "Auxerre" ~ "https://upload.wikimedia.org/wikipedia/fr/f/fe/Logo_AJ_Auxerre.svg",
    opponent == "Lille" ~ "https://upload.wikimedia.org/wikipedia/fr/6/62/Logo_LOSC_Lille_2018.svg",
    opponent == "Brest" ~ "https://upload.wikimedia.org/wikipedia/en/0/05/Stade_Brestois_29_logo.svg",
    opponent == "Reims" ~ "https://upload.wikimedia.org/wikipedia/commons/0/04/Stade_Reims.svg",
    opponent == "Troyes" ~ "https://upload.wikimedia.org/wikipedia/commons/b/bf/ES_Troyes_AC.svg"))

team_xG <- L1_22_stats %>%
  select( gameweek, team, opponent_logo, opponent, result, xG)
colnames(team_xG)[6] <- "xGxGA"

team_xGA <- L1_22_stats %>%
  select(gameweek, team, opponent_logo, opponent, result, xGA)
colnames(team_xGA)[6] <- "xGxGA"

team_xGxGA <- rbind(team_xG, team_xGA)

xg_mean <- team_xG %>%
  group_by(team) %>%
  summarise_at(vars(xGxGA), list(xg_mean = mean))

xga_mean <- team_xGA %>%
  group_by(team) %>%
  summarise_at(vars(xGxGA), list(xga_mean = mean))

xg_x<-xg_mean
xg_x$xg_mean<-0

team_xGdiffxGA <- team_xGxGA %>%
  group_by(team) %>%
  arrange(gameweek) %>%
  mutate(xGdiffxGA = xGxGA + lag(xGxGA, default = first(xGxGA))) %>%
  filter(row_number()%%2 == 0)
team_xGxGA$f_color <- ifelse(team_xGxGA$xGxGA > 0, "orange", "grey28")
team_xGxGA$b_color <- ifelse(team_xGxGA$xGxGA > 0, "grey28", "orange")
result_color <- ifelse(team_xGdiffxGA$result == "W", "green", ifelse(team_xGdiffxGA$result == "L","red","orange"))
last_gw = tail(team_xGxGA %>% group_by(gameweek) %>% filter(row_number() == n()), n = 1)

max_value <- max(abs(team_xGxGA$xGxGA))
max_value_int <- ceiling(max_value)
chart_lim <- max_value_int+1
ggplot(data = team_xGdiffxGA, aes(x = gameweek, y = -4)) +
  geom_point() +
  xlab("") +
  ylab("") +
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent", colour = NA),  
        plot.background = element_rect(fill = "transparent", colour = NA))+
  geom_bar(data = team_xGxGA, aes(x = gameweek, y = xGxGA), fill = team_xGxGA$f_color, color = team_xGxGA$b_color,
           size = 0, stat = "identity", show.legend = FALSE) +
  geom_point(data = team_xGdiffxGA, aes(x = gameweek, y = -4),
             color = "grey", show.legend = FALSE,
             shape = 21, stroke = 0.1, size = 0.1) +
  ggimage::geom_image(data = team_xGdiffxGA, aes(image=opponent_logo), size = 0.06) + scale_size_identity()+
  geom_point(data = team_xGdiffxGA, aes(x = gameweek, y = 4),
             color = result_color, show.legend = FALSE,
             shape = 21, stroke = 1.5, size = 10) +
  geom_text(data =team_xGdiffxGA, aes(x = gameweek, y = 4, label = result), size = 4, color=result_color)+
  geom_point(data = team_xGdiffxGA, aes(x = gameweek, y = xGdiffxGA),
             color = "black", show.legend = FALSE,
             shape = 21, stroke = 2, size = 3) +
  geom_line(data = team_xGdiffxGA, aes(x = gameweek, y = xGdiffxGA),
            size = 1.2, color = "black")+
  geom_hline(data = xg_mean, aes(yintercept = as.numeric(round(xg_mean,2))), color = "black", linetype='dotted') +
  geom_hline(data = xga_mean, aes(yintercept = as.numeric(round(xga_mean,2))), color = "black", linetype='dotted')+
  ggrepel::geom_label_repel(data = team_xGxGA %>% filter(xGxGA >= 0), aes(x = gameweek, y = xGxGA, label = xGxGA), nudge_y = 0.2, nudge_x = 0, size = 4,  min.segment.length = 10, color="white", fill="black") +
  ggrepel::geom_label_repel(data = team_xGxGA %>% filter(xGxGA < 0), aes(x = gameweek, y = xGxGA, label = abs(xGxGA)), nudge_y = -0.2, nudge_x = 0, size = 4,  min.segment.length = 10, color="white", fill="black")+
  coord_cartesian(ylim = c(-chart_lim, chart_lim)) + scale_x_continuous(breaks = 1:as.numeric(last_gw$gameweek)) + scale_y_continuous(breaks = c(as.numeric(round(xga_mean$xga_mean,2)),0,as.numeric(round(xg_mean$xg_mean,2)))) +
  theme(axis.text = element_text(size = 17, color = 'black'))


ggsave(filename = "xG_xGA.png",
       plot=last_plot(),
       bg='transparent',
       width=10,
       height=10)

