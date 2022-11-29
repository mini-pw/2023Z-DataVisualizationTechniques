library(stringr)         
library(ggplot2)
library(dplyr)

df<-read.csv(file = "Dane.csv", sep = ';')
df$prc<-as.numeric(gsub(",",".",df$prc))
df$prc<-df$prc*(-1)

df %>% 
  mutate(over_zero = ifelse(prc>0,"type1","type2")) %>% 
  ggplot(aes(x=dyscipline, y=prc)) +
  geom_segment( aes(x=dyscipline, xend=dyscipline, y=0, yend=prc, color=over_zero), size=30, alpha=0.9) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    axis.text.x = element_text(angle = 40, vjust = 0.9, hjust = 1)
  ) +
  xlab("") +
  ylab("Percentage difference in records between genders")+
  labs(title = "Percentage difference between women's and men's records in sports")+
  ylim(c(-0.3,0.1))+
  geom_hline(yintercept = 0, size = 1.2)+
  theme(text = element_text(size = 20), 
        plot.margin = margin(1,1,1.5,1.2, "cm"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  theme(axis.text = element_text(size = 20))->p
 
p 
#ggsave(p, filename = "plot1.svg")
