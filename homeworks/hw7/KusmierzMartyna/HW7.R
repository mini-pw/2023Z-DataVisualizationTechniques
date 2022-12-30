library(dplyr)
library(ggplot2)
library(gganimate)


tree <- list(
  matrix(
    c(1,1,9,1,7,3,8,3,6,5,7,5,5,7,3,5,4,5,2,3,3,3,1,1),
    ncol=2,
    byrow=T
))

tree <- sf::st_polygon(tree)
points = sf::st_sample(tree, size=80)

snow <- data.frame(
  x = runif(10000, 1, 9),
  y = runif(10000, 1, 7.5),
  state = rep(1:100, each = 100))

choinka <- ggplot() + 
  geom_sf(aes(), data=tree, fill = "green") + 
  geom_sf(aes(), data=points, color = "red", size=4) +
  geom_point(mapping = aes(x = 5, y = 7), color = 'gold', fill = 'gold', size = 11, shape = 22) +
  geom_point(mapping = aes(x = 5, y = 7), color = 'gold', fill = 'gold', size = 11, shape = 23) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  geom_point(data = snow, aes(x, y), size = 5, color = "white", pch = 42) +
  transition_states(snow$state, transition_length = 1, state_length = 1)

animate(choinka,fps=5)
anim_save("choinka.gif")
