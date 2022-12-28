library(dplyr)
library(ggplot2)
library(gganimate)

tree_d <- data.frame(x=c(1:9), y=c(seq(1,3, by=0.5),seq(2.5,1, by=-0.5))) %>% 
  na.omit() %>% 
  arrange(x)
tree_u <- data.frame(x=c(2:8), y=c(seq(2,3.5, by=0.5),seq(3,2, by=-0.5))) %>% 
  na.omit() %>% 
  arrange(x)
tree<-rbind(tree_d,tree_u)

trunk<- data.frame(x=c(4.5,5.5,5.5,4.5), y=c(1,1,0.8,0.8)) %>% 
  na.omit()

trunk_extra<- data.frame(x=c(5.3,4.45,5.45,5.45,4.4,5.3), y=c(0.9,0.97,0.97,0.83,0.83,0.87)) %>% 
  na.omit()

extra_d<-data.frame(x=c(seq(4.05,8.8,0.95),2.3,7.7), y=c(rep(1.95,3),seq(1.95,1, by=-0.45),1.05,1.1)) %>% 
  na.omit() 
  
extra_u<-data.frame(x=c(4.05,7,5.5,7.8), y=c(2.05,2.1,3.2,2.05)) %>% 
  na.omit()
line1<-data.frame(x=c(1.45,7.5), y=c(1.21,1.75))%>% 
  na.omit() 
line2<-data.frame(x=c(2.87,6.53), y=c(2.447,2.773))%>% 
  na.omit() 
lights<-data.frame(x=c(seq(1.45,7.5,0.75625),seq(2.87,6.53,0.61)),
                   y=c(1.21,1.2775,1.345,1.4125,1.48,1.5457,1.615,1.6825,1.75,2.447,2.501,2.5556,2.610,2.6645,2.719,2.773),
                   t=rep(c(1,2),8))

tree_d %>% 
  ggplot(aes(x=x,y=y))+
  geom_polygon(fill = "#009933")+#668c6f
  geom_polygon(data = tree_u, fill="#009933")+#668c6f
  geom_polygon(data = extra_d,fill="#004d1a")+#55735c
  geom_polygon(data = extra_u,fill="#004d1a")+#55735c
  geom_polygon(data = trunk,fill="#993300")+
  geom_polygon(data = trunk_extra,fill="#4d1a00")+
  geom_point(aes(x=5,y=3.5),colour = "#ebc90c", shape = 8,size=12,stroke=2)+
  geom_line(data=line1)+
  geom_line(data=line2)+#99ebff
  geom_point(data=lights,colour ="#00ffff", shape = 8,size= 4,stroke=1.5)+
  theme_classic()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background = element_rect(fill = '#001a4d'),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  transition_manual(lights$t)->anim
animate(anim,nframes=3)
