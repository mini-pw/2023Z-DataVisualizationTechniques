library(ggplot2)
library(dplyr)
library(ggstar)
set.seed(100)

gora = data.frame(x = c(-1,0,1), y = c(0,1,0))
dol = data.frame(x = c(-0.2,0.2), y = c(-0.15, -0.15))
data.frame(x = runif(100,-1, 1), y = runif(100,0,1), kolor = sample(1:5))%>%
  filter(y <= 1-abs(x))-> bombki
gw = filter(gora, x == 0)

ggplot()+
  geom_area(data = gora, aes(x = x, y = y),fill = "#165B33")+
  geom_area(data = dol, aes(x = x, y = y),fill = "#4B2013")+
  geom_point(data = bombki, aes(x = x, y = y, col = as.character(kolor)), size = 5)+
  scale_color_manual(values = c("#022F89","#BB2528","#F8B229","#F7F3D2","#970F4A"))+
  geom_star(gw, mapping = aes(x = x, y = y), colour = "#A57C00", fill = "#A57C00", size = 10)+
  theme_void()+theme(legend.position = "none")
