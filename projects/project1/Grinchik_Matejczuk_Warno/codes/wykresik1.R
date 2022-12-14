library(readxl)
library(dplyr)
library(ggplot2)
library(ggtext)
esportData <- read.csv("C:/Users/xxxx/Downloads/GeneralEsportData.csv")
topGames <- read_excel("C:/Users/xxxx/Downloads/TopGames.xlsx")
colnames(topGames)
topGames = rename(topGames,  "Prize" = "Total Prize Money (Year)")
tg = topGames %>% 
  group_by(Year) %>%
  summarise(max = max(Prize))
df = merge(topGames, tg)
df = df %>% 
  filter(Prize == max) 
df$max = df$max/10000
df$Game = c("Quake II", "StarCraft: Brood War", "Quake III Arena", "Counter-Strike", "Counter-Strike", "Counter-Strike", "Counter-Strike", "Counter-Strike", "Counter-Strike", "Counter-Strike", "Counter-Strike", "Counter-Strike", "Counter-Strike", "StarCraft II", "League of Legends", "League of Legends", "Dota 2", "Dota 2", "Dota 2", "Dota 2", "Dota 2", "Fortnite", "CS:GO")
df$Game <- factor(df$Game, levels = c("Quake II", "StarCraft: Brood War", "Quake III Arena", "Counter-Strike", "StarCraft II", "League of Legends", "Dota 2", "Fortnite", "CS:GO"))
ggplot(df, aes(x = Year, color = Game, fill = Game, y = max)) +
  xlim(1997, 2021) +
  geom_col()+
  scale_y_log10() +
  #scale_fill_discrete(breaks=legend_ord)+
  #scale_fill_discrete(limits = c("Quake II", "StarCraft: Brood War", "Quake III Arena", "Counter-Strike", "StarCraft II", "League of Legends", "Dota 2", "Fortnite", "CS:GO"))+
  labs(title = "Gry z najwieksza sumaryczna pula nagród w danym roku",
       subtitle = "1998-2020",
       x = "Rok",
       y = "Suma nagród (w 100 tys. USD)")+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot b
    legend.background = element_rect(fill='transparent'),#transparent legend bg
    legend.box.background = element_rect(fill='transparent'),
    legend.title = element_markdown(color = "white"),
    legend.text = element_markdown(color = "white"),
    panel.grid.major = element_line("transparent"),
    panel.grid.minor.x = element_line("transparent")
  ) +
  theme(
    axis.title = element_markdown(color = "white"),
    axis.text = element_markdown(color = "white"),
    axis.ticks = element_blank()
      )+
  theme(
    plot.title = element_markdown(color = "white"),
    plot.subtitle = element_markdown(color = "white")) -> plot

ggsave('TPNN.png', plot, bg = 'transparent', width = 5, height = 4)
plot
