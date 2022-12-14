# Wykres liczby poszczególnych medali w zale¿noœci od PKB per capita

# Za³¹czam pakiety

library(ggplot2)
library(dplyr)

# Wczytujê dane (liczba medali z igrzysk olimpiskich Rio 2016)

dane2 <- read.table(url("http://re-design.dimiter.eu/wp-content/uploads/2016/08/olypm.txt"), sep="\t", header=T)
dane2
dane2$gold[dane2$gold==0]<-NA
dane2$silver[dane2$silver==0]<-NA
dane2$bronze[dane2$bronze==0]<-NA
View(dane2)

#Modyfikujê dane, aby by³y lepiej widoczne na wykresie

dane2=dane2 %>% 
  mutate(gold=log(gold/popul*1000000000), silver=log(silver/popul*1000000000), bronze=log(bronze/popul*1000000000))

#Tworzê wykres

wykres<-dane2 %>% 
  ggplot(mapping = aes(x = gdppc, y=value, size=2))+
  geom_point(aes(y = bronze), color = "brown") +
  geom_point(aes(y = silver), color = "grey") +
  geom_point(aes(y = gold), color = "gold") + 
  xlim(0, 60000)+
  ylim(0, 10)+
  labs(x="GDP per capita", y="Number of medals per capita [1/bln] (logarithmic scale)", title="Sport success in relation to wealth")+
  scale_size(guide="none")+
  theme(rect = element_blank(),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size=10, color="black"),
        axis.text.y = element_text(size=10, color="black"),
        plot.title = element_text(size =30, hjust = 0.5),
        panel.grid.major.y = element_line(color="lightgrey"))

wykres               

