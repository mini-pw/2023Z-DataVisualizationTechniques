df <- read.csv("costs.csv")
View(df)
library(dplyr)
library(ggplot2)
library(scales)
options(scipen = 10)
df %>%
  mutate(year = substr(Olympics,1,4)) %>%
  ggplot(aes(x=year,y=Broadcast.Revenue, group = Season, color = Season)) +
  geom_line(size = 1.5) +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-9),
                     expand = c(0,0),
                     breaks = seq(0, 3*10^9, by = 0.5*10^9)) +
  scale_x_discrete(breaks = seq(1964,2018,6),expand = c(0,0)) +
  labs(title = "Przychody z transmisji igrzysk", 
       subtitle = "Lata 1964 - 2016", 
       y = "Przychody (mld USD)", 
       x = "Rok igrzysk",
       color = 'Pora roku') +
  scale_color_manual(labels = c("Letnie", "Zimowe"), values = c("#ef304d","#0082c9")) +
  theme(plot.background = element_rect(fill = "#323232",color = "#323232"),
        panel.background = element_rect(fill = "#323232"),
        legend.background = element_rect(fill = "#323232"),
        legend.key = element_rect(fill = "#323232"),
        text = element_text(colour = "#fcb22d"),
        axis.line = element_line(colour = "#fcb22d"),
        axis.text = element_text(colour = "#fcb22d", size = 12),
        axis.title = element_text(size = 14, colour = "#fcb22d", face = "bold"),
        panel.grid = element_line(colour = "#fcb22d"),
        panel.border = element_blank(),
        title = element_text(size = 18, face = "bold", colour = "#fcb22d")
  )
