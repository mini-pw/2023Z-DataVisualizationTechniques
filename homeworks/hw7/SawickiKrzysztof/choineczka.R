library(dplyr)
library(emojifont)
library(ggplot2)
set.seed(1345)

# Choinka
len <- (1000:1)/2000
drzewko <- data.frame(x = 0, y = 0)
y = 0
for(i in rep(exp(-(1:125)* 0.008), 8) * len + len){
  y <- y + 1
  for(x in seq(-i, i, 0.02)){
    drzewko %>% rbind(c(x,y)) -> drzewko
  }
}

#ozdoby
ozdoby_czerwone <- drzewko[runif(5,min = 30000, max = nrow(drzewko)),] %>% 
  rbind(drzewko[runif(17,min = 1, max = nrow(drzewko)),]) %>% 
  rbind(drzewko[runif(6,min = 1, max = 10000),])
ozdoby_zlote <- drzewko[runif(5,min = 30000, max = nrow(drzewko)),] %>% 
  rbind(drzewko[runif(15,min = 1, max = nrow(drzewko)),])%>% 
  rbind(drzewko[runif(6,min = 1, max = 10000),])
wstazki <- drzewko[runif(5,min = 30000, max = nrow(drzewko)),] %>% 
  rbind(drzewko[runif(17,min = 1, max = nrow(drzewko)),])


#Rysowanko
ggplot(drzewko,aes(x=x,y=y))+
  geom_rect(aes(xmin = -1.1, xmax = 1.1, ymin = -250, ymax = -30),fill = "#DCF0EE")+
  geom_emoji(
    alias = "snowflake",
    color = "white",
    size = 5,
    x = runif(100, min = -1.1, max = 1.1),
    y = runif(100, min = 0, max = 1000)
  )+
  geom_point(color = "#083002")+
  geom_rect(aes(xmin = -0.1, xmax = 0.1, ymin = -150, ymax = -5),fill = "#240103")+
  geom_rect(aes(xmin = 0.25, xmax = 0.5, ymin = -150, ymax = -40), fill = "red")+
  geom_rect(aes(xmin = 0.35, xmax = 0.4, ymin = -150, ymax = -40), fill = "yellow")+
  geom_rect(aes(xmin = 0.25, xmax = 0.5, ymin = -110, ymax = -80), fill = "yellow")+
  geom_rect(aes(xmin = -0.6, xmax = -0.4, ymin = -150, ymax = -10), fill = "#125F9C")+
  geom_rect(aes(xmin = -0.52, xmax = -0.48, ymin = -150, ymax = -10), fill = "yellow")+
  geom_rect(aes(xmin = -0.6, xmax = -0.4, ymin = -95, ymax = -65), fill = "yellow")+
  geom_rect(aes(xmin = -0.4, xmax = -0.4, ymin = -150, ymax = -10), fill = "#884AD6")+
  geom_rect(aes(xmin = -0.52, xmax = -0.48, ymin = -150, ymax = -10), fill = "yellow")+
  geom_rect(aes(xmin = -0.6, xmax = -0.4, ymin = -95, ymax = -65), fill = "yellow")+
  geom_rect(aes(xmin = -0.35, xmax = -0.10, ymin = -180, ymax = -30), fill = "#884AD6")+
  geom_rect(aes(xmin = -0.255, xmax = -0.20, ymin = -180, ymax = -30), fill = "yellow")+
  geom_rect(aes(xmin = -0.35, xmax = -0.10, ymin = -120, ymax = -90), fill = "yellow")+
  geom_fontawesome(
    alias = "fa-star",
    color = "gold",
    size = 15,
    x = 0,
    y = 1050
  )+
  geom_point(aes(x=x, y=y), data=ozdoby_czerwone, size=5, fill = "#891212",shape=21)+
  geom_point(aes(x=x, y=y), data=ozdoby_zlote, size=7, fill = "#DBC82B",shape=21)+
  geom_emoji(
    alias = "ribbon",
    color = "#4D1387",
    size = 5,
    x = wstazki[,1],
    y = wstazki[,2]
  )+
  theme_void()+
  theme(panel.background = element_rect(fill = '#5C81C2'))

        