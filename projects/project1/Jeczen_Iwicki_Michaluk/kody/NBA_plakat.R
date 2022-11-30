library(dplyr)
library(ggplot2)
library(patchwork)
library(readr)
library(measurements)
library(stringr)
library(stringi)

Players <- read_csv("projekt/Players.csv")
NBA2017_2018 <- read_csv("projekt/2017-2018NBA.csv", )
NBA_season1718_salary <- read_csv("projekt/NBA_season1718_salary.csv")

mutate(read_table2("projekt/2017-2018NBA.csv"), Player = paste(Name, Surname))->NBA2017_2018
# naprawa danych dla 3 cz³onowych nazw
for (i  in 1:length(NBA2017_2018$TEAM)){
  if (NBA2017_2018[i,"TEAM"] %in% c("Jr.", 'II', 'III', "IV")){
   NBA2017_2018[i,"HEIGHT"] = NBA2017_2018[i,"WEIGHT"]
   NBA2017_2018[i,"WEIGHT"] = NBA2017_2018[i,"COLLEGE"]
   NBA2017_2018[i,"Player"] = paste(NBA2017_2018[i,"Player"], NBA2017_2018[i,"TEAM"])
  }
}

#niektore ramki maja nazwy graczy z obcymi znakami 
mutate(read_csv("projekt/dane2017-2018.csv"), Player = stri_trans_general(Player, "Latin-ASCII"))->dane2017_2018

#sumuje statystyki dla graczy ktorzy grali w roznych dru¿ynach w sezonie
summarise(group_by(dane2017_2018,Player),Pos, G = sum(G), PTS = sum(PTS),
          TRB = sum(TRB), BLK = sum(BLK), AST = sum(AST), STL = sum(STL))%>%
distinct()->Stats2017_2018

dane2017_2018%>%
  inner_join(NBA_season1718_salary)-> ramka

#usuwam 2 posrednie pozycje
ramka%>%
  filter(Pos != 'PG-SG' & Pos!= 'SF-SG')%>%
  group_by(Pos)%>%
  summarise(avg = mean(season17_18)/1000000)%>%
  mutate(Pos = c("Center", "Power Forward", "Point Guard", "Small Forward", "Shooting Guard"))%>%
  mutate(Pos = forcats::fct_reorder(Pos, desc(avg)))%>%
  ggplot(mapping = aes(x = Pos, y = avg))+
  geom_col(fill = '#17408b')+
  labs(title =  "Average salary depending on position", 
       x = "Position", 
       y  = "Milions of dolars per season")+
  scale_x_discrete(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0,  0), limits = c(0, 8))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line( size=.1, color="gray"))
  

# zmiana jednostek wagi i wzrostu
distinct((select(Stats2017_2018, -Pos)))->Stats2017_2018

ungroup(inner_join(Stats2017_2018,select(NBA2017_2018, Player, HEIGHT,WEIGHT), 
                   by = c("Player" = "Player")))%>%
mutate(height = 0, weight = conv_unit(as.numeric(WEIGHT), 'lbs', 'kg')) -> w_h

str_split_fixed(w_h$HEIGHT, '-',  2)->feet_inches
mutate(w_h, height = conv_unit(as.numeric(feet_inches[,1]), 'feet', 'cm')+
         conv_unit(as.numeric(feet_inches[,2]), 'inch', 'cm'))->w_h

#ramka dla 10% najlepszych graczy w danej kategorii
w_h%>%
  arrange(desc(TRB))%>%
  filter(ntile(TRB, 10) == 10)%>%
  transmute(Skill = "Rebounds",height, weight)->w_h_skill

w_h%>%
  arrange(desc(BLK))%>%
  filter(ntile(BLK, 10) == 10)%>%
  transmute(Skill = "Blocks",height, weight)%>%
  bind_rows(w_h_skill)->w_h_skill

w_h%>%
  arrange(desc(AST))%>%
  filter(ntile(AST, 10) == 10)%>%
  transmute(Skill = "Asists",height, weight)%>%
  bind_rows(w_h_skill)->w_h_skill

w_h%>%
  arrange(desc(STL))%>%
  filter(ntile(STL, 10) == 10)%>%
  transmute(Skill = "Steals",height, weight)%>%
  bind_rows(w_h_skill)->w_h_skill

w_h_skill%>%
  group_by(Skill)%>%
  mutate(mean_height = mean(height), mean_weight= mean(weight))->w_h_skill

#wykres dla najlepszych 10%
rename(w_h_skill, Categories = Skill)%>%
ggplot(mapping = aes(x = weight, fill = Categories, colour = Categories))+
  geom_density(alpha = 0.6, lwd = 1.5)+
  geom_vline(mapping = aes(xintercept = mean_weight, colour = Categories), lwd = 1.5)+
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+theme(legend.position="top")+
  labs(title = "Density of distribution of weight and height of top 10% best players in each category",
    x = "Weight in kilograms", y = "Density")+
  ylim(0,0.1)+ 
  scale_fill_manual(values=c('#797EF6', '#1E2F97','#1AA7EC', '#4ADEDE'))+
  scale_color_manual(values=c('#797EF6', '#1E2F97','#1AA7EC', '#4ADEDE'))+
  theme(text = element_text(size = 30), legend.text   =element_text(size = 30) )-> w_dens

ggplot(w_h_skill,mapping = aes(x = height, fill = Skill, colour = Skill))+
  geom_density(alpha = 0.6,lwd = 1.5)+
  geom_vline(mapping = aes(xintercept = mean_height, colour = Skill), lwd = 1.5)+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_continuous(expand = c(0, 0))+
  theme(legend.position='none')+
  labs(x = "Height in centimeters", y = "Density")+ 
  ylim(0,0.1)+
  scale_fill_manual(values=c('#797EF6', '#1E2F97','#1AA7EC', '#4ADEDE'))+
  scale_color_manual(values=c('#797EF6', '#1E2F97','#1AA7EC', '#4ADEDE'))+
  theme(text = element_text(size = 30))-> h_dens
    
(w_dens/h_dens)

#ramka i wykres uzytecznosc od zarobkow
ramka%>%
  filter(G >=10)%>% #conajmniej 10 gier by nie brac pod uwage dlugo kontuzjowanych i lawkowiczow
  transmute(Salary = season17_18/1000000, Asists = AST/max(AST), Points = PTS/max(PTS),
            Rebounds = TRB/max(TRB), Steals = STL/ max(STL), Blocks = BLK/max(BLK))%>%
  arrange(Salary)%>%
  ggplot(mapping = aes(x = Salary, y = 100*(Points+Asists+Blocks+Steals+Rebounds)/5))+
  geom_point(color = '#c9082a')+
  geom_smooth(color = '#17408b', lwd = 1.5)+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,80))+
  labs(title = 'Usability depending on salary', x = "Salary in milions of dolars per season",
       y = 'Usability*(%)')+
  theme(text = element_text(size = 20))

  