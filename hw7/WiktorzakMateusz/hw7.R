library(ggplot2)
library(gganimate)
library(geomtextpath)

tree <- data.frame(
  x = c(0,1.8,0.7,1.5,0.5,1.2,0,-1.2,-0.5,-1.5,-0.7,-1.8,0),
  y = c(0,0,1,1,2,2,3,2,2,1,1,0,0),
  z = c(1,2,3,4,5,6,7,8,9,10,11,12,13)
)

balls <- data.frame(
  x = c(0.4, -0.1, -0.8, 0.7, 0.2,-0.5, 0.2),
  y = c(0.2, 2.1, 0.4, 1.5, 0.9, 1.3, 2.5)
)

star <- data.frame(
  x = c(1, 1.13, 1.25, 1.3, 2, 3, 2.8, 2.9, 2, 1.3, 1.45, 1.25, 1.15, 1.05, 0.85, 1, 1),
  y = c(2.8, 3, 2.8, 3.08, 3.2, 3, 3.2, 3.4, 3.3, 3.15, 3.3, 3.3, 3.5, 3.3, 3.3, 3.1, 2.8)
)

t = seq(-0.7, 0.5, by=0.1)

text1 <- data.frame(
  x = t,
  y = 1.75+1/10*(t+0.76)^2,
  t = "Wesołych"
)

text2 <- data.frame(
  x = t*1.7+0.4,
  y = 1.2+1/10*(t*1.6-1)^2,
  t = "Świąt"
)



ggplot(tree, aes(x,y)) +
  geom_polygon(fill = "darkgreen")  +
  geom_polygon(data = star, aes(x,y))+
  geom_point(data = balls, aes(x=x, y=y), size = 8, color = "darkred")+
  transition_reveal(z) +
  geom_textline(data = text1, aes(x,y, colour = t, label = t), size = 8, color = "orange", )+
  geom_textline(data = text2, aes(x,y, colour = t, label = t), size = 8, color = "orange")+
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "none")


