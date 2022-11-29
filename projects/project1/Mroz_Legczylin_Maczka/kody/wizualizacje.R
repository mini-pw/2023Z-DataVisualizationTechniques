install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
library(dplyr)
library(ggplot2)
library(tidyverse)

#MAPKA

athlete_events <- read.csv("./athlete_events.csv")
medalisci <- athlete_events
noc_regions <- read.csv("./noc_regions.csv")


medalisci <- left_join(medalisci, noc_regions, by = "NOC")

med1<-medalisci %>% 
  mutate(region=replace(region, region=="Soviet Union","Russia")) %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  count(region, name='Number_of_medals') %>% 
  arrange(desc("Number_of_medals"))

mapdata <- map_data("world")


mapdata <- left_join(mapdata, med1, by="region")

map1<-ggplot(mapdata, aes( x = long, y = lat, group=group)) +
  geom_polygon(aes(fill = Number_of_medals), color = "black")
  
map1

map2 <- map1 + scale_fill_gradient(name = "Liczba medali", low = "yellow", high =  "red", na.value = "white", trans = "log10")+
  theme(axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.y=element_blank(),
  axis.title.x=element_blank(),
  rect = element_blank(),
  panel.grid = element_blank(),
  panel.background = element_rect(fill = '#B8B8B8')) +
  labs(title = "Zdobyte medale od 1896 do 2016")
map2  

#KONIEC MAPKI

#DENSITY GRAPH + ILOSC MEDALI

medalisci<-read.csv(file='athlete_events.csv', header=TRUE,skip=0)
medalisci<-as.data.frame(medalisci)
head(medalisci)
medalisci<-medalisci %>% 
  mutate(NOC=replace(NOC, NOC=="URS","RUS"))
med1<-medalisci %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  count(NOC, name='Count') %>% 
  arrange(desc(Count)) %>% 
  head(3)

najwiecej_medali_pol<-medalisci %>% 
  filter(Team == "Poland") %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  group_by(Sport,Event,NOC,Year,Medal,Sex) %>% 
  summarise(medal=n()) %>% 
  mutate(medal=1) %>% 
  group_by(Sport,Sex) %>% 
  summarise(Ilosc_medali=n()) %>% 
  arrange(desc(Ilosc_medali))  %>% 
  head(3) %>% 
  mutate(kraj="Polska")

med2<-medalisci %>% 
  filter(Team == "Poland" | NOC %in% med1$NOC) %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  filter(Sport %in% najwiecej_medali_pol$Sport) %>% 
  filter(Sex %in% najwiecej_medali_pol$Sex) %>% 
  group_by(NOC,Sport,Sex) %>% 
  summarise(mean_height=mean(Height,na.rm=TRUE), mean_weight=mean(Weight,na.rm=TRUE))

med2

pol_medal_1<-medalisci %>% 
  filter(NOC=="POL") %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  group_by(Sport,Event,NOC,Year,Medal,Sex) %>% 
  summarise(medal=n()) %>% 
  mutate(medal=1) %>% 
  group_by(Sport,Sex) %>% 
  summarise(Ilosc_medali=n()) %>% 
  arrange(desc(Ilosc_medali))  %>% 
  mutate(kraj="Polska")
rename(Ilosc_medali_pol=Ilosc_medali)


niem_medal_1<-medalisci %>% 
  filter(NOC=="GER") %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  group_by(Sport,Event,NOC,Year,Medal,Sex) %>% 
  summarise(medal=n()) %>% 
  mutate(medal=1) %>% 
  group_by(Sport,Sex) %>% 
  summarise(Ilosc_medali=n()) %>% 
  arrange(desc(Ilosc_medali))  %>% 
  mutate(kraj="Niemcy") 

rename(Ilosc_medali_niem=Ilosc_medali)

do_gest<-inner_join(pol_medal_1,niem_medal_1, by=c("Sport","Sex")) %>% 
  group_by(Sport,Sex) %>% 
  mutate(różnica=abs(Ilosc_medali.x-Ilosc_medali.y)) %>% 
  arrange(desc(różnica)) %>% 
  head(3)

pol_medal_1<-medalisci %>% 
  filter(NOC=="POL") %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  group_by(Sport,Event,NOC,Year,Medal,Sex) %>% 
  summarise(medal=n()) %>% 
  mutate(medal=1) %>% 
  group_by(Sport,Sex) %>% 
  summarise(Ilosc_medali=n()) %>% 
  arrange(desc(Ilosc_medali))  %>% 
  mutate(kraj="Polska")
rename(Ilosc_medali_pol=Ilosc_medali)


niem_medal_1<-medalisci %>% 
  filter(NOC=="GER") %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  group_by(Sport,Event,NOC,Year,Medal,Sex) %>% 
  summarise(medal=n()) %>% 
  mutate(medal=1) %>% 
  group_by(Sport,Sex) %>% 
  summarise(Ilosc_medali=n()) %>% 
  arrange(desc(Ilosc_medali))  %>% 
  mutate(kraj="Niemcy") 

rename(Ilosc_medali_niem=Ilosc_medali)

do_gest<-inner_join(pol_medal_1,niem_medal_1, by=c("Sport","Sex")) %>% 
  group_by(Sport,Sex) %>% 
  mutate(różnica=abs(Ilosc_medali.x-Ilosc_medali.y)) %>% 
  arrange(desc(różnica)) %>% 
  head(3)

medalisci %>% 
  filter(NOC %in% c("POL")) %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  filter(Sport =="Canoeing") %>% 
  summarise(mean_h = mean(Height), mean_w=mean(Weight))
medalisci %>% 
  filter(NOC %in% c("GER")) %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  filter(Sport =="Canoeing") %>% 
  summarise(mean_h = mean(Height, na.rm=TRUE), mean_w=mean(Weight,na.rm=TRUE))


kaj<-  medalisci %>% 
  filter(NOC %in% c("RUS","USA","GER","POL","GBR","FRA","ITA","SWE","CAN","AUS", "HUN")) %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  filter(Sport =="Canoeing") %>% 
  filter(Sex %in% najwiecej_medali_pol$Sex) %>% 
  mutate(Sport=replace(Sport, Sport=="Canoeing","Kajakarstwo")) %>% 
  mutate(Sport=replace(Sport, Sport=="Equestrianism","Jeździectwo")) %>% 
  mutate(Sport=replace(Sport, Sport=="Swimming","Pływanie")) %>%
  ggplot(aes(x=Weight, y=Height))+
  stat_density_2d(aes(fill=..level..), geom = "polygon")+
  scale_fill_gradient(low="#9d9d9d", high="navy blue")+
  theme(legend.position = "none")+
  geom_point(aes(x=75.891897, y=175.6486), colour="red",size=3)+
  geom_point(aes(x=78.72414, y=181.0138), colour="black", size=3)+
  annotate("text",x=78.72414, y=181.0138, label = "Średni niemiecki atleta", vjust = -1, size=2.5, color="black")+
  annotate("text", x=75.891897, y=175.6486, label = "Średni polski atleta", vjust = 2, size=2.5, color="red")+theme(panel.background = element_rect(fill = '#b8b8b8', color='#b8b8b8'))+
  theme(panel.background = element_rect(fill = '#b8b8b8', color='#b8b8b8'))+
  theme(plot.background = element_rect(fill = '#b8b8b8', color='#b8b8b8'))+
  labs(y = "Wzrost", x ="Waga")+
  xlim(60, 100)+
  ylim(160, 200)
medalisci %>% 
  filter(NOC %in% c("POL")) %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  filter(Sport =="Equestrianism") %>% 
  summarise(mean_h = mean(Height,na.rm=TRUE), mean_w=mean(Weight,na.rm=TRUE))
medalisci %>% 
  filter(NOC %in% c("GER")) %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  filter(Sport =="Equestrianism") %>% 
  summarise(mean_h = mean(Height, na.rm=TRUE), mean_w=mean(Weight,na.rm=TRUE))

jez<- medalisci %>% 
  filter(NOC %in% c("RUS","USA","GER","POL","GBR","FRA","ITA","SWE","CAN","AUS", "HUN")) %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  filter(Sport =="Equestrianism") %>% 
  filter(Sex %in% najwiecej_medali_pol$Sex) %>% 
  mutate(Sport=replace(Sport, Sport=="Canoeing","Kajakarstwo")) %>% 
  mutate(Sport=replace(Sport, Sport=="Equestrianism","Jeździectwo")) %>% 
  mutate(Sport=replace(Sport, Sport=="Swimming","Pływanie")) %>%
  ggplot(aes(x=Weight, y=Height))+
  stat_density_2d(aes(fill=..level..), geom = "polygon")+
  scale_fill_gradient(low="#9d9d9d", high="navy blue")+
  theme(legend.position = "none")+
  geom_point(aes(x=68.76923, y=172.5385 ), colour="red",size=3)+
  geom_point(aes(x=68.18269, y=176.0481 ), colour="black", size=3)+
  annotate("text", x=68.18269, y=176.0481, label = "Średni niemiecki atleta", vjust = -1, size=2.5, color="black")+
  annotate("text", 68.76923, y=172.5385, label = "Średni polski atleta", vjust = 2, size=2.5, color="red")+
  theme(panel.background = element_rect(fill = '#b8b8b8', color='#b8b8b8'))+
  theme(plot.background = element_rect(fill = '#b8b8b8', color='#b8b8b8'))+
  labs(y = "Wzrost", x ="Waga")+
  xlim(60, 100)+
  ylim(160, 200)

medalisci %>% 
  filter(NOC %in% c("POL")) %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  filter(Sport =="Swimming") %>% 
  summarise(mean_h = mean(Height,na.rm=TRUE), mean_w=mean(Weight,na.rm=TRUE))
medalisci %>% 
  filter(NOC %in% c("GER")) %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  filter(Sport =="Swimming") %>% 
  summarise(mean_h = mean(Height, na.rm=TRUE), mean_w=mean(Weight,na.rm=TRUE))

plyw <-medalisci %>% 
  filter(NOC %in% c("RUS","USA","GER","POL","GBR","FRA","ITA","SWE","CAN","AUS", "HUN")) %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  filter(Sport =="Swimming") %>% 
  filter(Sex %in% najwiecej_medali_pol$Sex) %>% 
  mutate(Sport=replace(Sport, Sport=="Canoeing","Kajakarstwo")) %>% 
  mutate(Sport=replace(Sport, Sport=="Equestrianism","Jeździectwo")) %>% 
  mutate(Sport=replace(Sport, Sport=="Swimming","Pływanie")) %>%
  ggplot(aes(x=Weight, y=Height))+
  stat_density_2d(aes(fill=..level..), geom = "polygon")+
  scale_fill_gradient(low="#9d9d9d", high="navy blue")+
  theme(legend.position = "none")+
  geom_point(aes(x=73.66667, y=184.8333 ), colour="red",size=3)+
  geom_point(aes(x=73.11404, y=183.5391 ), colour="black", size=3)+
  annotate("text", x=73.11404, y=183.5391, label = "Średni niemiecki atleta", vjust = 2, size=2.5, color="black")+
  annotate("text", 73.66667, y=184.8333, label = "Średni polski atleta", vjust = -1, size=2.5, color="red")+theme(panel.background = element_rect(fill = '#b8b8b8', color='#b8b8b8'))+
  theme(panel.background = element_rect(fill = '#b8b8b8', color='#b8b8b8'))+
  theme(plot.background = element_rect(fill = '#b8b8b8', color='#b8b8b8'))+
  labs(y = "Wzrost", x ="Waga")+
  xlim(60, 100)+
  ylim(160, 200)
library(patchwork)
kaj+jez+plyw

do_gest<-inner_join(pol_medal_1,niem_medal_1, by=c("Sport","Sex")) %>% 
  group_by(Sport,Sex) %>% 
  mutate(różnica=abs(Ilosc_medali.x-Ilosc_medali.y)) %>% 
  arrange(desc(różnica)) %>% 
  head(3)

najwiecej_medali_pol<-medalisci %>% 
  filter(Team == "Poland") %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  group_by(Sport,Event,NOC,Year,Medal,Sex) %>% 
  summarise(medal=n()) %>% 
  mutate(medal=1) %>% 
  group_by(Sport,Sex) %>% 
  summarise(Ilosc_medali=n()) %>% 
  arrange(desc(Ilosc_medali))  %>% 
  filter(Sex == "M") %>% 
  filter(Sport %in% do_gest$Sport) %>% 
  mutate(kraj="Polska")

najwiecej_medali_niem<-medalisci %>% 
  filter(Team == "Germany") %>% 
  filter(Medal %in% c('Bronze', 'Silver', 'Gold')) %>% 
  group_by(Sport,Event,NOC,Year,Medal,Sex) %>% 
  summarise(medal=n()) %>% 
  mutate(medal=1) %>% 
  group_by(Sport,Sex) %>% 
  summarise(Ilosc_medali=n()) %>% 
  arrange(desc(Ilosc_medali))  %>% 
  filter(Sex == "M") %>% 
  filter(Sport %in% do_gest$Sport) %>% 
  mutate(kraj="Niemcy")

por_niem_pol <- rbind(najwiecej_medali_niem, najwiecej_medali_pol)
por_niem_pol<-por_niem_pol %>% 
  mutate(Sport=replace(Sport, Sport=="Swimming","Pływanie")) %>% 
  mutate(Sport=replace(Sport, Sport=="Equestrianism","Jeździectwo")) %>% 
  mutate(Sport=replace(Sport, Sport=="Canoeing","Kajakarstwo"))

ggplot(data=por_niem_pol, aes(fill=kraj, y=Ilosc_medali, x=Sport)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=c("black", "red"))+
  coord_flip()+
  labs(y = "Ilość medali", fill="Kraje")+
  theme(panel.background = element_rect(fill = '#b8b8b8', color='#b8b8b8'))+
  theme(plot.background = element_rect(fill = '#b8b8b8', color='#b8b8b8'))+
  theme(legend.key = element_rect(fill = "#b8b8b8", color='#b8b8b8'))+
  theme(legend.background = element_rect(fill = "#b8b8b8", color='#b8b8b8'))

#LINIOWY

df <- na.omit(read.csv("athlete_events.csv"))

df %>%
  filter(Team == "Poland") %>%
  group_by(Year) %>%
  summarise(count = n()) %>%
  
  ggplot(aes(x = Year, y = count)) +
  scale_x_discrete(limits = seq(from = 1920,
                                to = 2020,
                                by = 16)) +
  geom_line(color = "#000000", size = 2) +
  
  labs(x = "Rok",
       y = "Liczba Polaków") +
  theme(
    axis.title = element_text(size = 40),
    axis.text = element_text(size = 20),
    plot.background = element_rect("#b8b8b8"),
    panel.background = element_rect("#b8b8b8")
  )


df %>%
  mutate(isPole = ifelse(Team == "Poland", "true", "false")) %>%
  group_by(Year, isPole) %>%
  summarise(count = n()) %>%
  group_by(Year) %>%
  mutate(ratioToAll = count / sum(count) * 100) %>%
  ungroup() %>%
  filter(isPole == "true") %>%
  
  ggplot(aes(x = Year, y = ratioToAll)) +
  scale_x_discrete(limits = seq(from = 1920,
                                to = 2020,
                                by = 16)) +
  geom_line(color = "#000000", size = 2) +
  
  labs(x = "Rok",
       y = "Liczba Polaków") +
  theme(
    axis.title = element_text(size = 40),
    axis.text = element_text(size = 20),
    plot.background = element_rect("#b8b8b8"),
    panel.background = element_rect("#b8b8b8")
  )

