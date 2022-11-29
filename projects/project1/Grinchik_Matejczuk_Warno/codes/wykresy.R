library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)
library(ggtext)
df <- read.csv('~/esportData/HistoricalEsportData.csv')
df2 <- read.csv('~/esportData/GeneralEsportData.csv')
options(scipen = 999)

df %>% filter(as.numeric(substr(Date, 1,4)) >= 2016) %>% group_by(Game) %>%
  summarise(n = sum(Earnings)) %>%  arrange(desc(n)) %>% head(5) -> top5games

###wszystkie wykresy poza ostatnim robione na latach 2016-2021

#rozproszenie podzialu nagród dla top5 najwiekszych gier pod wzgledem puli nagród
#dla kazdego miesiaca liczymy ile przypada pieniêdzy na gracza (laczne nagrody w tym msc/liczba graczy, którzy otrzymali nagrody)
#po czym liczymy z tego mediane
#oczywiscie nagrody w niektorych grach (np w Docie) sa wieksze niz w innych, dlatego aby otrzymac szacowany
#wskaznik "plaskosci" podzialu nagród obliczona w poprzednim kroku mediane dzielimy przez mediane sumy pienieznej przypadajacej na turniej

df %>% filter(as.numeric(substr(Date, 1,4)) >= 2016) %>% filter(Game %in% c(top5games$Game)) %>% 
  mutate(cash_per_player = Earnings/Players, cash_per_tournament = Earnings/Tournaments) %>% 
  group_by(Game) %>% summarise(median_cash_per_player = median(cash_per_player), median_cash_per_tournament = median(cash_per_tournament))-> tmp
tmp[5,1] <- "PUBG"
tmp$Game <- factor(tmp$Game, levels = c("Counter-Strike: Global Offensive", "League of Legends","Dota 2","Fortnite", "PUBG"))
tmp %>% 
  ggplot(aes(x = fct_infreq(Game), y = median_cash_per_player/median_cash_per_tournament)) + 
    geom_col(fill = "lightblue") + 
  labs(title = "W której grze podzial nagród jest najbardziej plaski?",
       subtitle = "2016-2021",
       x= "Nazwa gry",
       y= "Wskaznik") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) + 
  theme_light()

#liczba turniejów dla kazdego miesiaca (raczej nieprzydatny)
df %>% filter(as.numeric(substr(Date, 1,4)) >= 2016) %>% mutate(month = as.numeric(substr(Date, 6, 7))) %>%
  filter(Earnings/Tournaments > 750000) %>% 
  ggplot(aes(x = month, y=mean(Tournaments))) + 
  geom_col(fill = "lightblue") + 
  labs(title = "Œrednia liczba turniejów esportowych o pulach nagród ponad 750k USD",
       subtitle = "2016-2021",
       x = "Miesi¹c",
       y = "Œrednia liczba turniejów") + 
  scale_x_discrete(limit = month.abb) + 
  theme_light()

#³laczna pula nagród dla kazdego miesiaca (raczej nieprzydatny)
df %>% filter(as.numeric(substr(Date, 1,4)) >= 2016) %>% mutate(month = as.numeric(substr(Date, 6, 7))) %>%
  filter(Earnings/Tournaments > 750000) %>% 
  ggplot(aes(x = month, y=mean(Earnings)/1000000)) + 
  geom_col(fill = "lightblue") + 
  labs(title = "Œrednia wysokoœæ puli nagród dla wszystkich turniejów \n esportowych o puli nagród ponad 750k USD",
       subtitle = "2016-2021",
       x = "Miesi¹c",
       y = "Œrednia pula nagród (w mln USD)") + 
  scale_x_discrete(limit = month.abb) +
  scale_y_continuous(expand= c(0,0))+
  theme_light() + 
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot b
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  ) +
  theme(
    axis.title = element_markdown(color = "white"),
    axis.text = element_markdown(color = "white"),
    panel.grid.major = element_line(color = "transparent"),
    panel.grid.minor = element_line(color = "white"),
    panel.border = element_rect(color = "white"),
    #plot.title = element_markdown(color = "white"),
    plot.subtitle = element_markdown(color = "white"))-> plot
ggsave('plot1.png', plot, bg = 'transparent')


#Laczna pula nagród dla kazdego roku (widac pandemie i kryzys z 2008)
df %>% mutate(Year = as.integer(substr(Date, 1,4))) %>% 
mutate(Earnings_w_inflation = case_when(
  Year == 1998 ~ Earnings*1.612,
  Year == 1999 ~ Earnings*1.588,
  Year == 2000 ~ Earnings*1.554,
  Year == 2001 ~ Earnings*1.503,
  Year == 2002 ~ Earnings*1.462,
  Year == 2003 ~ Earnings*1.439,
  Year == 2004 ~ Earnings*1.407,
  Year == 2005 ~ Earnings*1.37,
  Year == 2006 ~ Earnings*1.325,
  Year == 2007 ~ Earnings*1.284,
  Year == 2008 ~ Earnings*1.248,
  Year == 2009 ~ Earnings*1.202,
  Year == 2010 ~ Earnings*1.206,
  Year == 2011 ~ Earnings*1.187,
  Year == 2012 ~ Earnings*1.15,
  Year == 2013 ~ Earnings*1.127,
  Year == 2014 ~ Earnings*1.111,
  Year == 2015 ~ Earnings*1.093,
  Year == 2016 ~ Earnings*1.092,
  Year == 2017 ~ Earnings*1.078,
  Year == 2018 ~ Earnings*1.056,
  Year == 2019 ~ Earnings*1.031,
  Year == 2020 ~ Earnings*1.012,
  Year == 2021 ~ Earnings)) %>% 
  
ggplot(aes(x = Year, y=sum(Earnings_w_inflation)/1000000000)) + 
  geom_col(fill = "lightblue") + 
  labs(title = "Laczna pula nagród ze wszystkich turniejów esportowych",
       subtitle = "1998-2021, z uwzglednieniem inflacji w stosunku do roku 2021*",
       caption = "*sumy nagród pomnozono przez wskaznik Consumer Price Index US Bureau of Labor Statistics",
       x = "Rok",
       y = "Laczna pula nagród (w mld USD)") +
  theme_light()


