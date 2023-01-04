library(gganimate)
library(transformr)
library(ggplot2)

df_tree <- rbind(
  data.frame(x = c(10,10,14,14,10), y = c(0,2,2,0,0),group = "1", fill = "sienna4" ),
  data.frame(x = c(0,12, 24, 0), y = c(2,14,2,2), group = "2",  fill = "green4"),
  data.frame(x = c(2,12,22,2), y = c(5,16,5,5), group = "3", fill = "green3"),
  data.frame(x = c(4,12,20,4), y = c(8,18,8,8), group = "4", fill = "green2"),
  data.frame(x = c(6,12,18,6), y = c(11,20,11,11), group = "5", fill = "green1"),
  data.frame(x = c(8,12,16,8), y = c(14,22,14,14), group = "6", fill = "chartreuse1")
)

x <- rbind(matrix(rnorm(100, mean=12, sd=10), ncol=2), matrix(rnorm(100, mean=12, sd=10), ncol=2))
df_snow <- as.data.frame(x)

p <- ggplot() + 
  geom_polygon(data = df, alpha = 1, aes(x = x, y = y,group = group, fill ="white", colour = fill, size = 1.8), show.legend = FALSE) +
  scale_colour_identity() + scale_fill_identity() +
  coord_fixed() + 
  theme_void()+
  transition_layers(layer_length = 1, transition_length = 10, keep_layers = TRUE, from_blank = TRUE) +
  enter_grow() +
  geom_point(data = df_snow, aes(x = V1, y = V2), fill = "white", colour = "lightcyan3", size = 1.5, stroke = 1, shape = 21) + 
  labs(title = "Merry Christmas, lots of food!") + 
  theme(title = element_text(colour = "red", size = 15, hjust = 1))
animate(p,24)
anim_save("choinka.gif")
