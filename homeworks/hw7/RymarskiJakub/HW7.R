library(ggplot2)
library(dplyr)
library(gganimate)
#install.packages("gganimate")

rysunek<-data.frame(x = c(rep(0, 7), -0.03, -0.02, 0.01, 0.02, 0.02, -0.02),
                y = c(0, 0.25, 0.6, 1.1, 1.6, 2, 2.3, rep(-0.02, 6)),
                shape = c(15, 15, rep(17, 4), 8, rep(15, 4), 12, 12),
                color = c("brown", "brown", "#31a354", "#31a354", "#31a354", "#31a354", "gold", "yellow", "red", "#de77ae", "blue", "yellow", "green"),
                size = c(20, 20, 60, 50, 40, 30, 20, 20, 20, 20, 20, 20, 20))

bombki = data.frame(x=runif(20, -0.01, 0.01),
                    y=runif(20, 0.5, 2),
                    pos=as.factor(c("a",'a','a','a','a','a','a','a', 'a', 'a',
                                    'b','b','b','b','b','b','b','b', 'b', 'b')),
                    col=c(rep("red", 7), rep("yellow", 7), rep("blue", 6)))

p1 <- ggplot(bombki) +
  geom_point(aes(x, y), rysunek,
             color =rysunek$color,
             shape = rysunek$shape,
             size = rysunek$size)+
  xlim(-0.2, 0.2)+
  labs(title="Wesołych Świąt")+
  theme(
      plot.title = element_text(hjust = 0.5, size=20, color="gold"),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.y=element_blank(),
      panel.background = element_blank())+
    geom_point(aes(x=x, y=y), bombki, shape=19, size= 5, color=bombki$col)+
    transition_states(pos, transition_length = 1, state_length = 0.01, wrap=TRUE)

animation <- animate(p1, nframes = 20, fps=100)

anim_save("choinka.gif", animation)



                 