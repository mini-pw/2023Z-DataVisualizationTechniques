champs<-read.csv("champs.csv")


library(dplyr)
library(ggplot2)
library(tidyverse)
library(mapproj)
library(patchwork)

# TWORZENIE WYKRESÓW DO ANKIETY

require(maps)
?map_data

champs<-as.data.frame(champs %>% 
                        group_by(champ) %>% 
                        summarise(wins=n()) %>% 
                        mutate(winratio=wins/1000000, wins=NULL) %>% 
                        arrange(-winratio))

world <- map_data("world")

vals<-data.frame('champ'=unique(world$region), 'winratio'=NA) %>% 
  filter(!(champ %in% champs$champ)) %>% 
  rbind(champs) %>% 
  rename(region=champ)

wrld<-world %>% 
  left_join(vals)

ggplot(wrld, aes(long, lat, group=group, fill=winratio)) +
  geom_polygon()->map

map+coord_map('mercator')+scale_fill_gradient(
  low = "#FFEEEE",
  high = "#FF0000",
  space = "Lab",
  na.value = "grey80",
  guide = "colourbar",
  aesthetics = "fill",
  labels=scales::percent,
  guide_axis(title='% of wins'))+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank()) 

map+coord_map('mercator')+scale_fill_gradient(
  low = "#D3B1B1",
  high = "#AA7979",
  space = "Lab",
  na.value = "grey80",
  guide = "colourbar",
  aesthetics = "fill",
  labels=scales::percent,
  guide_axis(title='% of wins'))+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())
  
ggplot(champs, aes(x=reorder(champ,-winratio), y=winratio))+
  geom_bar(stat="identity", width = 0.69)+
  scale_y_continuous(labels=scales::percent,
                     guide_axis(title='% of wins'),
                     expand=c(0,0,0.04, 0.04))+
  scale_x_discrete(guide=guide_axis(title='team', angle=50))+
  scale_fill_manual(values=colors) -> graph

graph + theme_minimal()+
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  geom_text(aes(label = scales::percent(winratio,1)),
            position=position_dodge(width=0.9), vjust=-0.5,
            size=3)
  


# ANALIZA ANKIET
ankiety_wyniki<-read.csv("ankiety_wyniki.csv")

m1q1<-as.data.frame(table(
  factor(ankiety_wyniki$Które.europejskie.państwo.osiągnęło.największy...zwycięstw.wg.danych.z.powyższej.mapy.
         , levels = c("Holandia", "Niemcy", "Francja", "Hiszpania", "Belgia", "Portugalia", "Nie mogę stwierdzić"))))
m1q2<-as.data.frame(table(
  factor(ankiety_wyniki$Które.z.tych.państw.osiągnęło.większy...zwycięstw.wg.danych.z.tej.mapy.
         , levels = c("USA", "Kanada", "Nie mogę stwierdzić"))))
m1q3<-as.data.frame(table(
  factor(ankiety_wyniki$Które.z.tych.państw.osiągnęło.większy...zwycięstw.wg.danych.z.tej.mapy..1
         , levels = c("Brazylia", "Argentyna", "Nie mogę stwierdzić"))))
m2q1<-as.data.frame(table(
  factor(ankiety_wyniki$Które.europejskie.państwo.osiągnęło.największy...zwycięstw.wg.danych.z.powyższej.mapy..1
  , levels = c("Holandia", "Niemcy", "Francja", "Hiszpania", "Belgia", "Portugalia", "Nie mogę stwierdzić"))))
m2q2<-as.data.frame(table(
  factor(ankiety_wyniki$Które.z.tych.państw.osiągnęło.większy...zwycięstw.wg.danych.z.tej.mapy..2
         , levels = c("USA", "Kanada", "Nie mogę stwierdzić"))))
m2q3<-as.data.frame(table(
  factor(ankiety_wyniki$Które.z.tych.państw.osiągnęło.większy...zwycięstw.wg.danych.z.tej.mapy..3
         , levels = c("Brazylia", "Argentyna", "Nie mogę stwierdzić"))))
m3q1<-as.data.frame(table(
  factor(ankiety_wyniki$Które.europejskie.państwo.osiągnęło.największy...zwycięstw.wg.danych.z.powyższego.wykresu.
  , levels = c("Holandia", "Niemcy", "Francja", "Hiszpania", "Belgia", "Portugalia", "Nie mogę stwierdzić"))))
m3q2<-as.data.frame(table(
  factor(ankiety_wyniki$Które.z.tych.państw.osiągnęło.większy...zwycięstw.wg.danych.z.tego.wykresu.
         , levels = c("USA", "Kanada", "Nie mogę stwierdzić"))))
m3q3<-as.data.frame(table(
  factor(ankiety_wyniki$Które.z.tych.państw.osiągnęło.większy...zwycięstw.wg.danych.z.tego.wykresu..1
         , levels = c("Brazylia", "Argentyna", "Nie mogę stwierdzić"))))

q10<-as.data.frame(table(
  ankiety_wyniki$Na.której.mapie.łatwiej.porównywało.się.wartości.))

q11<-as.data.frame(table(
  ankiety_wyniki$Czy.wykres.słupkowy.był.dużym.ułatwieniem.w.odczytywaniu.wartości.))

##########################################

m1q1 %>% 
  ggplot(aes(x=Var1,y=(Freq/22),
         fill=factor(ifelse(Var1=="Holandia","Poprawna odpowiedź","Błędna odpowiedź"))))+
  geom_bar(stat = 'identity', width = 0.69)+
  geom_text(aes(label = scales::percent((Freq/22))),
            position=position_dodge(width=0.9), vjust=-0.5)+
  ggtitle('z danych na mapie 1')+
  scale_y_continuous(labels=scales::percent,
                     guide_axis(title='% ankietowanych'),
                     expand=c(0,0,0.04, 0.04),
                     limits = c(0,0.7))+
  scale_x_discrete(guide=guide_axis(title='wskazana odpowiedź', angle=40))+
  scale_fill_manual(name = "Var1", values=c("gray50","darkgreen"))+
  theme_bw()+
  theme(legend.position = "none")->p1q1

m2q1 %>% 
  ggplot(aes(x=Var1,y=(Freq/22),
             fill=factor(ifelse(Var1=="Holandia","Poprawna odpowiedź","Błędna odpowiedź"))))+
  geom_bar(stat = 'identity', width = 0.69)+
  geom_text(aes(label = scales::percent((Freq/22))),
            position=position_dodge(width=0.9), vjust=-0.5)+
  ggtitle('z danych na mapie 2')+
  scale_y_continuous(labels=scales::percent,
                     guide_axis(title='% ankietowanych'),
                     expand=c(0,0,0.04, 0.04),
                     limits = c(0,0.7))+
  scale_x_discrete(guide=guide_axis(title='wskazana odpowiedź', angle=40))+
  scale_fill_manual(name = "Var1", values=c("gray50","darkgreen"))+
  theme_bw()+
  theme(legend.position = "none")->p2q1

m3q1 %>% 
  ggplot(aes(x=Var1,y=(Freq/22),
             fill=factor(ifelse(Var1=="Holandia","Poprawna odpowiedź","Błędna odpowiedź"))))+
  geom_bar(stat = 'identity', width = 0.69)+
  geom_text(aes(label = scales::percent((Freq/22))),
            position=position_dodge(width=0.9), vjust=-0.5)+
  ggtitle('z danych na wykresie słupkowym')+
  scale_y_continuous(labels=scales::percent,
                     guide_axis(title='% ankietowanych'),
                     expand=c(0,0,0.04, 0.04),
                     limits = c(0,0.7))+
  scale_x_discrete(guide=guide_axis(title='wskazana odpowiedź', angle=40))+
  scale_fill_manual(name = "Var1", values=c("gray50","darkgreen"))+
  theme_bw()+
  theme(legend.position = "none")->p3q1

##########################################

m1q2 %>% 
  ggplot(aes(x=Var1,y=(Freq/22),
             fill=factor(ifelse(Var1=="USA","Poprawna odpowiedź","Błędna odpowiedź"))))+
  geom_bar(stat = 'identity', width = 0.69)+
  geom_text(aes(label = scales::percent((Freq/22))),
            position=position_dodge(width=0.9), vjust=-0.5)+
  ggtitle('z danych na mapie 1')+
  scale_y_continuous(labels=scales::percent,
                     guide_axis(title='% ankietowanych'),
                     expand=c(0,0,0.04, 0.04),
                     limits = c(0,1))+
  scale_x_discrete(guide=guide_axis(title='wskazana odpowiedź', angle=40))+
  scale_fill_manual(name = "Var1", values=c("gray50","darkgreen"))+
  theme_bw()+
  theme(legend.position = "none")->p1q2

m2q2 %>% 
  ggplot(aes(x=Var1,y=(Freq/22),
             fill=factor(ifelse(Var1=="USA","Poprawna odpowiedź","Błędna odpowiedź"))))+
  geom_bar(stat = 'identity', width = 0.69)+
  geom_text(aes(label = scales::percent((Freq/22))),
            position=position_dodge(width=0.9), vjust=-0.5)+
  ggtitle('z danych na mapie 2')+
  scale_y_continuous(labels=scales::percent,
                     guide_axis(title='% ankietowanych'),
                     expand=c(0,0,0.04, 0.04),
                     limits = c(0,1))+
  scale_x_discrete(guide=guide_axis(title='wskazana odpowiedź', angle=40))+
  scale_fill_manual(name = "Var1", values=c("gray50","darkgreen"))+
  theme_bw()+
  theme(legend.position = "none")->p2q2

m3q2 %>% 
  ggplot(aes(x=Var1,y=(Freq/22),
             fill=factor(ifelse(Var1=="USA","Poprawna odpowiedź","Błędna odpowiedź"))))+
  geom_bar(stat = 'identity', width = 0.69)+
  geom_text(aes(label = scales::percent((Freq/22))),
            position=position_dodge(width=0.9), vjust=-0.5)+
  ggtitle('z danych na wykresie słupkowym')+
  scale_y_continuous(labels=scales::percent,
                     guide_axis(title='% ankietowanych'),
                     expand=c(0,0,0.04, 0.04),
                     limits = c(0,1))+
  scale_x_discrete(guide=guide_axis(title='wskazana odpowiedź', angle=40))+
  scale_fill_manual(name = "Var1", values=c("gray50","darkgreen"))+
  theme_bw()+
  theme(legend.position = "none")->p3q2

##########################################

m1q3 %>% 
  ggplot(aes(x=Var1,y=(Freq/22),
             fill=factor(ifelse(Var1=="Brazylia","Poprawna odpowiedź","Błędna odpowiedź"))))+
  geom_bar(stat = 'identity', width = 0.69)+
  geom_text(aes(label = scales::percent((Freq/22))),
            position=position_dodge(width=0.9), vjust=-0.5)+
  ggtitle('z danych na mapie 1')+
  scale_y_continuous(labels=scales::percent,
                     guide_axis(title='% ankietowanych'),
                     expand=c(0,0,0.04, 0.04),
                     limits = c(0,1))+
  scale_x_discrete(guide=guide_axis(title='wskazana odpowiedź', angle=40))+
  scale_fill_manual(name = "Var1", values=c("gray50","darkgreen"))+
  theme_bw()+
  theme(legend.position = "none")->p1q3

m2q3 %>% 
  ggplot(aes(x=Var1,y=(Freq/22),
             fill=factor(ifelse(Var1=="Brazylia","Poprawna odpowiedź","Błędna odpowiedź"))))+
  geom_bar(stat = 'identity', width = 0.69)+
  geom_text(aes(label = scales::percent((Freq/22))),
            position=position_dodge(width=0.9), vjust=-0.5)+
  ggtitle('z danych na mapie 2')+
  scale_y_continuous(labels=scales::percent,
                     guide_axis(title='% ankietowanych'),
                     expand=c(0,0,0.04, 0.04),
                     limits = c(0,1))+
  scale_x_discrete(guide=guide_axis(title='wskazana odpowiedź', angle=40))+
  scale_fill_manual(name = "Var1", values=c("gray50","darkgreen"))+
  theme_bw()+
  theme(legend.position = "none")->p2q3

m3q3 %>% 
  ggplot(aes(x=Var1,y=(Freq/22),
             fill=factor(ifelse(Var1=="Brazylia","Poprawna odpowiedź","Błędna odpowiedź"))))+
  geom_bar(stat = 'identity', width = 0.69)+
  geom_text(aes(label = scales::percent((Freq/22))),
            position=position_dodge(width=0.9), vjust=-0.5)+
  ggtitle('z danych na wykresie słupkowym')+
  scale_y_continuous(labels=scales::percent,
                     guide_axis(title='% ankietowanych'),
                     expand=c(0,0,0.04, 0.04),
                     limits = c(0,1))+
  scale_x_discrete(guide=guide_axis(title='wskazana odpowiedź', angle=40))+
  scale_fill_manual(name = "Var1", values=c("gray50","darkgreen"))+
  theme_bw()+
  theme(legend.position = "none")->p3q3


p1q1+p2q1+p3q1+
  plot_annotation(title='Który europejski kraj ma największy % zwycięstw?')

p1q2+p2q2+p3q2+
  plot_annotation(title='Kanada czy USA ma większy % zwycięstw?')

p1q3+p2q3+p3q3+
  plot_annotation(title='Brazylia czy Argentyna ma większy % zwycięstw?')

  
