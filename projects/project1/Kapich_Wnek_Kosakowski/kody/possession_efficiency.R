library(dplyr)
library(ggplot2)
library(ggimage)
library(scales)

shooting <- read.csv('shooting.csv')
possession <- read.csv('possession.csv')

names(shooting) <- shooting[1,]
names(possession) <- possession[1,]

shooting <- shooting[-1,]
possession <- possession[-1,]

df <- inner_join(shooting, possession, by = 'Squad') %>% 
  mutate(image = paste("./Ligue_1_pngs/", Squad, '.png')) %>% 
  mutate(image = gsub(' ', '', image))

colnames(df)[which(names(df) == 'np:G-xG')] <- 'npG_xG'

df %>% 
  mutate(npG_xG = as.numeric(npG_xG),
         Poss = as.numeric(Poss)) -> df
 
df %>%
   ggplot(aes(npG_xG, Poss)) +
   geom_image(aes(image=image), size = 0.06) +
   #ggtitle("Ligue 1 2022/2023",
           #subtitle="Ball possession in relation to team efficiency") +
   theme(aspect.ratio=1) +
   scale_x_continuous(breaks = seq(-8, 9, 1)) + 
   scale_y_continuous(breaks = seq(40, 65, 2.5)) +
   #coord_cartesian(ylim=c(40,61.25))
   xlab("") +
   ylab("") +
   theme_linedraw() +
   theme(panel.background = element_rect(fill = "transparent", colour = NA),  
         plot.background = element_rect(fill = "transparent", colour = NA)) +
   # theme(axis.line = element_line(arrow = arrow())) +
   theme(aspect.ratio=1) +
  labs(caption="") +
  theme(axis.text = element_text(size = 17, color = 'black'))
 
 
 ggsave(filename='possession_efficiency.png',
        plot=last_plot(),
        bg='transparent',
        width=10,
        height=10)
 