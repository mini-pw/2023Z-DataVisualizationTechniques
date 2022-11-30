library(dplyr)
library(ggplot2)
library(ggrepel)

# koszta <- read.csv("immakym.csv")
zawodnicy <- read.csv("data/athlete_events.csv")
countries <- read.csv("data/country_definitions.csv")
hosts <- read.csv("data/hosts.csv")


# Suma wszystkich medali
zawodnicy %>% 
  filter(!is.na(Medal),
         Season == "Summer") %>% 
  group_by(Team, Year) %>% 
  summarise(TeamSum = n()) -> teams_by_year


# Suma medali hostów
zawodnicy %>% 
  inner_join(hosts, by ="City") %>% 
  mutate(isHost = ifelse(Team == Country, T, F)) %>% 
  filter(!is.na(Medal),
         Season == "Summer",
         Team %in% hosts$Country,
         isHost == T) %>% 
  group_by(Year.x, Team) %>% 
  summarise(hostMedals = n()) %>% 
  rename(Year = Year.x) -> hosts_sum

# Dane do boxplotu

  ggplot() +
    geom_boxplot(data =  teams_by_year, mapping = aes(y = as.factor(Year), x = TeamSum, col = "Reszta"), fill = "white", lwd = 0.9, outlier.size = 2.5, show.legend = FALSE) +
    geom_point(data = hosts_sum, mapping = aes(y = as.factor(Year), x = hostMedals, col = "Organizator"), size = 5) +
    scale_colour_manual("", 
                        breaks = c("Organizator", "Reszta"),
                        values = c("#ef304d", "#0082c9")) +
    
    scale_x_log10(limits = c(1,1e3),expand = c(0,0)) +
    labs( title = "Porównanie liczby zdobytych medali w igrzyskach letnich",
          subtitle = "Lata 1896 - 2016",
          x = "Liczba medali",
          y = "Rok") +
    coord_flip() +
    theme_bw() +
    theme(plot.background = element_rect(fill = "#323232",color = "#323232"),
          panel.background = element_rect(fill = "#323232"),
          legend.background = element_rect(fill = "#323232"),
          legend.key = element_rect(fill = "#323232"),
          legend.text = element_text(face = "bold"),
          text = element_text(colour = "#fcb22d", size = 26),
          axis.line = element_line(colour = "#fcb22d"),
          axis.title = element_text(size = 22),
          axis.text.x = element_text(colour = "#fcb22d", size = 16),
          axis.text.y = element_text(colour = "#fcb22d", size = 16),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"),
          panel.grid = element_line(colour = "#fcb22d", size = 0.5),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 28, face = "bold"),
          panel.border = element_blank()
          )
  