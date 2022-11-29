library(showtext)
library(dplyr)
library(ggplot2)
library(tidyr) 

#zaczytanie ramek
real22 <- read.csv2("reality21-22.csv")
fifa23 <- read.csv("players_fifa23.csv")
font_add("knul", "C:/Users/sebas/OneDrive/Dokumenty/R/Projek_1_TWD/Knul-Regular.otf")
showtext_auto()

real_statystyki <- real22 %>% 
  filter((Pos == "FW"|Pos == "FWDF"| Pos == "FWMF")) %>%
  filter(Age != 40) %>%
  select(Goals,Min,G.Sh,Shots,PasTotAtt,PasShoCmp.,Pas3rd,PasAss,ScaDrib,Press, DriSucc.,DriAtt,AerWon,AerWon.,Age,Rk)

real_kwantyle <- real22 %>% 
  filter((Pos == "FW"|Pos == "FWDF"| Pos == "FWMF")) %>%
  filter(Age != 40) %>%
  select(Goals,Min,G.Sh,Shots,PasTotAtt,PasShoCmp.,Pas3rd,PasAss,ScaDrib,Press, DriSucc.,DriAtt,AerWon,AerWon.,Age) %>%
  summarise(Gole = quantile(as.numeric(Goals),probs = seq(0.01, 1, 1/100)),
            Minuty = quantile(as.numeric(Min),probs = seq(0.01, 1, 1/100)),
            GoleStrzal = quantile(as.numeric(G.Sh),probs = seq(0.01, 1, 1/100)),
            Strzaly = quantile(as.numeric(Shots),probs = seq(0.01, 1, 1/100)),
            Podania = quantile(as.numeric(PasTotAtt),probs = seq(0.01, 1, 1/100)),
            PodaniaCelneProcent = quantile(as.numeric(PasShoCmp.),probs = seq(0.01, 1, 1/100)),
            PodaniaW3Kwarte = quantile(as.numeric(Pas3rd),probs = seq(0.01, 1, 1/100)),
            WykreowanieOkazji = quantile(as.numeric(PasAss),probs = seq(0.01, 1, 1/100)),
            DryblingDoStrzlu = quantile(as.numeric(ScaDrib),probs = seq(0.01, 1, 1/100)),
            Pressing = quantile(as.numeric(Press),probs = seq(0.01, 1, 1/100)),
            DryblingizSukcesemProcent = quantile(as.numeric(DriSucc.),probs = seq(0.01, 1, 1/100)),
            DryblingiSuma = quantile(as.numeric(DriAtt),probs = seq(0.01, 1, 1/100)),
            WygranePowietrze = quantile(as.numeric(AerWon),probs = seq(0.01, 1, 1/100)),
            ProcentWygranegoPowietrza = quantile(as.numeric(AerWon.),probs = seq(0.01, 1, 1/100)))

ktory_kwantyl<-function(a,b){
  return (which.min2(abs(b - a)))
}

which.min2 <- function(y){
  return(max(which(y == min(y))))
}
real_statystyki_suma <- real_statystyki %>%
  summarise(Age=Age,
            Rk = Rk,
            kwantyl1 = as.numeric(lapply(as.numeric(Goals), ktory_kwantyl,real_kwantyle$Gole)),
            kwantyl2 = as.numeric(lapply(as.numeric(Min), ktory_kwantyl,real_kwantyle$Minuty)),
            kwantyl3 = as.numeric(lapply(as.numeric(G.Sh), ktory_kwantyl,real_kwantyle$GoleStrzal)),
            kwantyl4 = as.numeric(lapply(as.numeric(Shots), ktory_kwantyl,real_kwantyle$Strzaly)),
            kwantyl5 = as.numeric(lapply(as.numeric(PasTotAtt), ktory_kwantyl,real_kwantyle$Podania)),
            kwantyl6 = as.numeric(lapply(as.numeric(PasShoCmp.), ktory_kwantyl,real_kwantyle$PodaniaCelneProcent)),
            kwantyl7 = as.numeric(lapply(as.numeric(Pas3rd), ktory_kwantyl,real_kwantyle$PodaniaW3Kwarte)),
            kwantyl8 = as.numeric(lapply(as.numeric(PasAss), ktory_kwantyl,real_kwantyle$WykreowanieOkazji)),
            kwantyl9 = as.numeric(lapply(as.numeric(ScaDrib), ktory_kwantyl,real_kwantyle$DryblingDoStrzlu)),
            kwantyl10 = as.numeric(lapply(as.numeric(Press), ktory_kwantyl,real_kwantyle$Pressing)),
            kwantyl11 = as.numeric(lapply(as.numeric(DriSucc.), ktory_kwantyl,real_kwantyle$DryblingizSukcesemProcent)),
            kwantyl12 = as.numeric(lapply(as.numeric(DriAtt), ktory_kwantyl,real_kwantyle$DryblingiSuma)),
            kwantyl13 = as.numeric(lapply(as.numeric(AerWon), ktory_kwantyl,real_kwantyle$WygranePowietrze)),
            kwantyl14 = as.numeric(lapply(as.numeric(AerWon.), ktory_kwantyl,real_kwantyle$ProcentWygranegoPowietrza)))%>%
  group_by(Rk)%>%
  summarise(Age = Age,Reality = mean(c(kwantyl1,kwantyl2,kwantyl3,kwantyl4,kwantyl5,kwantyl6,kwantyl7,kwantyl8,kwantyl9,kwantyl10,kwantyl11,kwantyl12,kwantyl13,kwantyl14)))


######
fifa23TOP5 <- fifa23 %>%
  filter(Club == "Real Madrid CF"|Club == "FC Barcelona"|Club == "Sevilla FC"|Club == "Atlético de Madrid"|Club == "Villarreal CF"|Club == "RC Celta de Vigo"|Club == "Real Betis Balompié"|Club == "Athletic Club de Bilbao"|Club == "Real Sociedad"|Club == "Getafe CF"|
           Club =="RCD Espanyol de Barcelona"|Club == "Valencia CF"|Club =="Cádiz CF"|Club == "Elche CF"|Club == "CA Osasuna"|Club == "Rayo Vallecano"|Club == "Deportivo Alavés"|Club == "RCD Mallorca"|Club == "Levante Unión Deportiva"|Club == "Granada CF"|
           Club =="Manchester City"|Club == "Liverpool"|Club =="Manchester United"|Club == "Chelsea"|Club == "Tottenham Hotspur"|Club == "Leicester City"|Club == "Arsenal"|Club == "Aston Villa"|Club == "Newcastle United"|Club == "West Ham United"|
           Club =="Wolverhampton Wanderers"|Club == "Crystal Palace"|Club =="Everton"|Club == "Southampton"|Club == "Leeds United"|Club == "Brighton & Hove Albion"|Club == "Brentford"|Club == "Watford"|Club == "Burnley"|Club == "Norwich City"|
           Club =="Paris Saint-Germain"|Club == "AS Monaco"|Club =="OGC Nice"|Club == "Olympique de Marseille"|Club == "Olympique Lyonnais"|Club == "Montpellier Hérault SC"|Club == "Racing Club de Lens"|Club == "Stade Rennais FC"|Club == "FC Nantes"|Club == "LOSC Lille"|
           Club =="FC Girondins de Bordeaux"|Club == "AS Saint-Étienne"|Club =="FC Lorient"|Club == "FC Metz"|Club == "ESTAC Troyes"|Club == "Stade de Reims"|Club == "Stade Brestois 29"|Club == "Clermont Foot 63"|Club == "Angers SCO"|Club == "RC Strasbourg Alsace"|
           Club =="FC Bayern München"|Club == "Eintracht Frankfurt"|Club =="RB Leipzig"|Club == "Bayer 04 Leverkusen"|Club == "Borussia Dortmund"|Club == "Borussia Mönchengladbach"|Club == "VfL Wolfsburg"|Club == "TSG Hoffenheim"|Club == "Sport-Club Freiburg"|Club == "1. FC Köln"|
           Club =="SpVgg Greuther Fürth"|Club == "DSC Arminia Bielefeld"|Club =="Hertha BSC"|Club == "FC Augsburg"|Club == "1. FSV Mainz 05"|Club == "1. FC Union Berlin"|Club == "VfB Stuttgart"|Club == "VfL Bochum 1848"|
           Club =="AC Milan"|Club == "Inter"|Club =="Juventus"|Club == "Lazio"|Club == "Roma"|Club == "U.S. Sassuolo Calcio"|Club == "Atalanta"|Club == "Napoli"|Club == "Bologna"|Club == "Fiorentina"|
           Club =="Udinese Calcio"|Club == "US Salernitana 1919"|Club =="Genoa"|Club == "Venezia FC"|Club == "Cagliari"|Club == "Spezia"|Club == "U.C. Sampdoria"|Club == "Torino F.C."|Club == "Hellas Verona"|Club == "Empoli")

fifa_kwantyle <- fifa23TOP5 %>%
  filter(Age!= 40)%>%
  filter(ClubPosition == "ST" |ClubPosition == "FW"|ClubPosition=="LW"|ClubPosition=="CF"| ClubPosition == "RW"|
           BestPosition == "ST" |BestPosition == "FW"|BestPosition=="LW"|BestPosition=="CF" |BestPosition == "RW") %>%
    summarise(centyle = quantile((Overall),probs = seq(0.01, 1, 1/100)))

fifa_statystyki <- fifa23TOP5 %>%
  filter(Age!= 40)%>%
  filter(ClubPosition == "ST" |ClubPosition == "FW"|ClubPosition=="LW"|ClubPosition=="CF"| ClubPosition == "RW"|
           BestPosition == "ST" |BestPosition == "FW"|BestPosition=="LW"|BestPosition=="CF" | BestPosition == "RW") %>%
  select(Age,Overall,ID)
  
fifa_statystyki_suma<-fifa_statystyki %>%
  summarise(Age = Age,
            Fifa = as.numeric(lapply(as.numeric(Overall), ktory_kwantyl,fifa_kwantyle$centyle)))
  

######


wykres_real <- real_statystyki_suma %>%
  select(Age,Reality) %>%
  pivot_longer(!Age,names_to = "wartosc", values_to = "centyle")

wykres_fifa <- fifa_statystyki_suma %>%
  select(Age,Fifa) %>%
  pivot_longer(!Age,names_to = "wartosc", values_to = "centyle")


wykres<- rbind(wykres_fifa, wykres_real)
my_white <- rgb(255, 255, 255, 150, maxColorValue = 255)

wiek_centyle <- ggplot(wykres, aes(x=Age, y=centyle, group =wartosc, color = wartosc)) +
  geom_jitter(height = 0, width = 0.2 ,alpha = 0.5,size = 1) +
  geom_smooth(se=FALSE) +
  labs(x = "Age", 
       y = "Centiles", color = "")+ 
  scale_color_manual(
         values = c("#ccf53f", "#cb3df0")) +
  theme(
    rect = element_rect(fill = 'transparent'),
    text = element_text(family = 'knul'),
    panel.background = element_rect(fill = 'transparent'),
    panel.grid.major.y = element_line(size = 0.2, color = my_white),
    panel.grid.minor.y = element_line(size = 0.2, color = my_white),
    panel.grid.major.x = element_line(size = 0.2, color = my_white),
    panel.grid.minor.x = element_line(size = 0.2, color = my_white),
    panel.border = element_blank(),
    axis.text.x = element_text(size = 30, color = 'white'),
    axis.text.y = element_text(size = 30, color = 'white'),
    axis.title = element_text(size = 35, color = 'white'),
    axis.ticks = element_blank(),
    legend.text = element_text(size = 30, color = 'white'),
    legend.key = element_blank(),
    legend.title = element_blank()
    )
wiek_centyle

ggsave(plot = wiek_centyle, file = "wiek_centyle.png",
       type = "cairo-png",  bg = "transparent",
       width = 1800, height = 1300, units = 'px')