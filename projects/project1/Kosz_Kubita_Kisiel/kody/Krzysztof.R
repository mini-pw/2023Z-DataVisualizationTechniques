df<-read.csv('wydatki.csv')
library('ggplot2')
library('dplyr')

df<-mutate(df, koszt=koszt.w.mil.pomn/1000)
df<-mutate(df, miastko=case_when(
  miasto=="Monachium"~"Munich",
  miasto=="Moskwa"~"Moscow",
  miasto=="Seoul"~"Seul",
  miasto=="Ateny"~"Athens",
  miasto=="Pekin"~"Beijing",
  miasto=="Londyn"~"London",
  TRUE~miasto
))
df<-mutate(df, nowe=paste(rok, miastko, sep = "\n"))


df%>%
  ggplot(aes(x=nowe, y=koszt))+
  geom_col(fill="darkgoldenrod2")+
  scale_y_continuous(breaks = seq(0,20, by=2.5))+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  labs(x="",
       y="")+
  theme(panel.grid=element_line(colour="black"))+
  theme(axis.text = element_text(size = 13))+
  ggsave(
    filename = "wydatki.png",
    bg = "transparent"
  )

  
