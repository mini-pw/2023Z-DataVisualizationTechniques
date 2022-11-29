df <- read.csv("athlete_events.csv")
getwd()
ls()
setwd('twd')
library(dplyr)
library(ggplot2)

head(df)
df %>% 
  filter(Sport=="Swimming") %>% 
  filter(is.na(Medal)==F) %>% 
  filter(Year>1990) %>% 
  mutate(Bronze= 
              ifelse(Medal=="Bronze", 1,0)) %>%
  mutate(Silver= 
           ifelse(Medal=="Silver", 1,0)) %>% 
  mutate(Gold= 
           ifelse(Medal=="Gold", 1,0)) %>% 
  group_by(Name) %>% 
  select(Name, Bronze, Silver, Gold) %>% 
  mutate(suma= Bronze+Silver+Gold) %>% 
  
  mutate(bronze_sum= sum(Bronze), silver_sum= sum(Silver), gold_sum= sum(Gold)) %>% 
  select(Name, bronze_sum, silver_sum, gold_sum) %>% 
         group_by(Name) %>%
         unique() %>%
         mutate(suma= bronze_sum+silver_sum+gold_sum) %>% 
  
        arrange(-suma) %>% 
  
        head() ->df1


#^ ogólnie to wszystko co wyżej okazało się nieprdzydatne i wykorzystuje to tylko
#żeby dostać nazwiska top zawodników


source("themes.r")
df$Medal <- factor(df$Medal, levels=c('Gold', 'Silver', 'Bronze'))

df %>% 
  filter(Sport=="Swimming") %>% 
  filter(is.na(Medal)==F) %>% 
  filter(Year>1990) %>% 
  mutate(Count= ifelse(is.na(Medal)==F, 1 ,0)) %>% 
  select(Name, Medal, Count) %>% 
  filter(Name %in% nazwiska) %>% 
  ggplot(aes(fill=Medal, x=Count, y=Name)) +
  scale_x_continuous(limits=c(0,30), expand=c(0,0)) +
  labs(x="Number of medals")+
  
  scale_y_discrete(limits= rev(nazwiska), labels=c("Franziska van Alsmick", "Gary Wayne Hall, Jr.", "Jennifer Thompson", "Ryan Steven Lochte", "Natalie Anne Coughlin", "Michael Fred Phelps"), expand=c(0,0)) +
  scale_fill_manual(values = (c("#f5c713","grey","#b2a0a0")))+
  theme_dark_blue()+
  geom_bar(position = "stack", stat="identity", width = rep(0.5,6)) +
  theme(axis.text=element_text(size=12, colour = "white"), legend.text = element_text(size =14 ), legend.title= element_text(size=14), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
  


 

  




