library(ggplot2)
library(gganimate)
library(dplyr)
library(transformr)
library(gifski)


df_generate <- function(a, b, h) {
  data.frame(
    x = rep(a, times = h + 1),
    y = rep(b, times = h + 1),
    pos = rep(0:h, each = length(a))
  ) -> df
  df %>% 
    mutate(y = y + pos)
}

shape1 <- df_generate(c(-1, 0, 0,-3,-3,-1), c(0, 0, 2, 2, 1, 1), 9)
shape2 <- df_generate(c(1, 0, 0, 3, 3, 1), c(0, 0, 2, 2, 1, 1), 9)
shape3 <- df_generate(c(-2, 2, 2,-2), c(2, 2, 3, 3), 8)
shape4 <- df_generate(c(-1, 1, 1, 0, 0, -2, -2, -1), c(3, 3, 4, 4, 5, 5, 4, 4), 7)
shape5 <- df_generate(c(0, 2, 2, 1, 1, -1, -1, 0), c(4, 4, 5, 5, 6, 6, 5, 5), 6)
shape6 <- df_generate(c(-1.5, 1.5, 1.5, 0.5, 0.5, -0.5, -0.5, -1.5), c(6, 6, 7, 7, 8, 8, 7, 7), 5)

ggplot() + 
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_fixed(ratio = 1) +
  labs(
    title = "Tetris christmas tree",
    x = "",
    y = ""
  ) +
  # naprawdę nie wiem dlaczego, ale z jakiegoś powodu nie działa mi z pętlą :(
  
  geom_polygon(aes(x = shape1[shape1$pos == 9,]$x, y = shape1[shape1$pos == 9,]$y),
               fill = "darkgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape1[shape1$pos == 9,]$x, y = shape1[shape1$pos == 9,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape1[shape1$pos == 8,]$x, y = shape1[shape1$pos == 8,]$y),
               fill = "darkgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape1[shape1$pos == 8,]$x, y = shape1[shape1$pos == 8,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape1[shape1$pos == 7,]$x, y = shape1[shape1$pos == 7,]$y),
               fill = "darkgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape1[shape1$pos == 7,]$x, y = shape1[shape1$pos == 7,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape1[shape1$pos == 6,]$x, y = shape1[shape1$pos == 6,]$y),
               fill = "darkgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape1[shape1$pos == 6,]$x, y = shape1[shape1$pos == 6,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape1[shape1$pos == 5,]$x, y = shape1[shape1$pos == 5,]$y),
               fill = "darkgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape1[shape1$pos == 5,]$x, y = shape1[shape1$pos == 5,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape1[shape1$pos == 4,]$x, y = shape1[shape1$pos == 4,]$y),
               fill = "darkgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape1[shape1$pos == 4,]$x, y = shape1[shape1$pos == 4,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape1[shape1$pos == 3,]$x, y = shape1[shape1$pos == 3,]$y),
               fill = "darkgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape1[shape1$pos == 3,]$x, y = shape1[shape1$pos == 3,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape1[shape1$pos == 2,]$x, y = shape1[shape1$pos == 2,]$y),
               fill = "darkgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape1[shape1$pos == 2,]$x, y = shape1[shape1$pos == 2,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape1[shape1$pos == 1,]$x, y = shape1[shape1$pos == 1,]$y),
               fill = "darkgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape1[shape1$pos == 1,]$x, y = shape1[shape1$pos == 1,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape1[shape1$pos == 0,]$x, y = shape1[shape1$pos == 0,]$y),
               fill = "darkgreen",
               color = "black",
               size = 1.5) +
  
  geom_polygon(aes(x = shape2[shape2$pos == 9,]$x, y = shape2[shape2$pos == 9,]$y),
               fill = "forestgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape2[shape2$pos == 9,]$x, y = shape2[shape2$pos == 9,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape2[shape2$pos == 8,]$x, y = shape2[shape2$pos == 8,]$y),
               fill = "forestgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape2[shape2$pos == 8,]$x, y = shape2[shape2$pos == 8,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape2[shape2$pos == 7,]$x, y = shape2[shape2$pos == 7,]$y),
               fill = "forestgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape2[shape2$pos == 7,]$x, y = shape2[shape2$pos == 7,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape2[shape2$pos == 6,]$x, y = shape2[shape2$pos == 6,]$y),
               fill = "forestgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape2[shape2$pos == 6,]$x, y = shape2[shape2$pos == 6,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape2[shape2$pos == 5,]$x, y = shape2[shape2$pos == 5,]$y),
               fill = "forestgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape2[shape2$pos == 5,]$x, y = shape2[shape2$pos == 5,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape2[shape2$pos == 4,]$x, y = shape2[shape2$pos == 4,]$y),
               fill = "forestgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape2[shape2$pos == 4,]$x, y = shape2[shape2$pos == 4,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape2[shape2$pos == 3,]$x, y = shape2[shape2$pos == 3,]$y),
               fill = "forestgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape2[shape2$pos == 3,]$x, y = shape2[shape2$pos == 3,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape2[shape2$pos == 2,]$x, y = shape2[shape2$pos == 2,]$y),
               fill = "forestgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape2[shape2$pos == 2,]$x, y = shape2[shape2$pos == 2,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape2[shape2$pos == 1,]$x, y = shape2[shape2$pos == 1,]$y),
               fill = "forestgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape2[shape2$pos == 1,]$x, y = shape2[shape2$pos == 1,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape2[shape2$pos == 0,]$x, y = shape2[shape2$pos == 0,]$y),
               fill = "forestgreen",
               color = "black",
               size = 1.5) +
  
  geom_polygon(aes(x = shape3[shape3$pos == 8,]$x, y = shape3[shape3$pos == 8,]$y),
               fill = "springgreen4",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape3[shape3$pos == 8,]$x, y = shape3[shape3$pos == 8,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape3[shape3$pos == 7,]$x, y = shape3[shape3$pos == 7,]$y),
               fill = "springgreen4",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape3[shape3$pos == 7,]$x, y = shape3[shape3$pos == 7,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape3[shape3$pos == 6,]$x, y = shape3[shape3$pos == 6,]$y),
               fill = "springgreen4",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape3[shape3$pos == 6,]$x, y = shape3[shape3$pos == 6,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape3[shape3$pos == 5,]$x, y = shape3[shape3$pos == 5,]$y),
               fill = "springgreen4",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape3[shape3$pos == 5,]$x, y = shape3[shape3$pos == 5,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape3[shape3$pos == 4,]$x, y = shape3[shape3$pos == 4,]$y),
               fill = "springgreen4",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape3[shape3$pos == 4,]$x, y = shape3[shape3$pos == 4,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape3[shape3$pos == 3,]$x, y = shape3[shape3$pos == 3,]$y),
               fill = "springgreen4",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape3[shape3$pos == 3,]$x, y = shape3[shape3$pos == 3,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape3[shape3$pos == 2,]$x, y = shape3[shape3$pos == 2,]$y),
               fill = "springgreen4",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape3[shape3$pos == 2,]$x, y = shape3[shape3$pos == 2,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape3[shape3$pos == 1,]$x, y = shape3[shape3$pos == 1,]$y),
               fill = "springgreen4",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape3[shape3$pos == 1,]$x, y = shape3[shape3$pos == 1,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape3[shape3$pos == 0,]$x, y = shape3[shape3$pos == 0,]$y),
               fill = "springgreen4",
               color = "black",
               size = 1.5) +
  
  geom_polygon(aes(x = shape4[shape4$pos == 7,]$x, y = shape4[shape4$pos == 7,]$y),
               fill = "darkgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape4[shape4$pos == 7,]$x, y = shape4[shape4$pos == 7,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape4[shape4$pos == 6,]$x, y = shape4[shape4$pos == 6,]$y),
               fill = "darkgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape4[shape4$pos == 6,]$x, y = shape4[shape4$pos == 6,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape4[shape4$pos == 5,]$x, y = shape4[shape4$pos == 5,]$y),
               fill = "darkgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape4[shape4$pos == 5,]$x, y = shape4[shape4$pos == 5,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape4[shape4$pos == 4,]$x, y = shape4[shape4$pos == 4,]$y),
               fill = "darkgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape4[shape4$pos == 4,]$x, y = shape4[shape4$pos == 4,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape4[shape4$pos == 3,]$x, y = shape4[shape4$pos == 3,]$y),
               fill = "darkgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape4[shape4$pos == 3,]$x, y = shape4[shape4$pos == 3,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape4[shape4$pos == 2,]$x, y = shape4[shape4$pos == 2,]$y),
               fill = "darkgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape4[shape4$pos == 2,]$x, y = shape4[shape4$pos == 2,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape4[shape4$pos == 1,]$x, y = shape4[shape4$pos == 1,]$y),
               fill = "darkgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape4[shape4$pos == 1,]$x, y = shape4[shape4$pos == 1,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape4[shape4$pos == 0,]$x, y = shape4[shape4$pos == 0,]$y),
               fill = "darkgreen",
               color = "black",
               size = 1.5) +
  
  geom_polygon(aes(x = shape5[shape5$pos == 6,]$x, y = shape5[shape5$pos == 6,]$y),
               fill = "forestgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape5[shape5$pos == 6,]$x, y = shape5[shape5$pos == 6,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape5[shape5$pos == 5,]$x, y = shape5[shape5$pos == 5,]$y),
               fill = "forestgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape5[shape5$pos == 5,]$x, y = shape5[shape5$pos == 5,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape5[shape5$pos == 4,]$x, y = shape5[shape5$pos == 4,]$y),
               fill = "forestgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape5[shape5$pos == 4,]$x, y = shape5[shape5$pos == 4,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape5[shape5$pos == 3,]$x, y = shape5[shape5$pos == 3,]$y),
               fill = "forestgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape5[shape5$pos == 3,]$x, y = shape5[shape5$pos == 3,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape5[shape5$pos == 2,]$x, y = shape5[shape5$pos == 2,]$y),
               fill = "forestgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape5[shape5$pos == 2,]$x, y = shape5[shape5$pos == 2,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape5[shape5$pos == 1,]$x, y = shape5[shape5$pos == 1,]$y),
               fill = "forestgreen",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape5[shape5$pos == 1,]$x, y = shape5[shape5$pos == 1,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape5[shape5$pos == 0,]$x, y = shape5[shape5$pos == 0,]$y),
               fill = "forestgreen",
               color = "black",
               size = 1.5) +
  
   geom_polygon(aes(x = shape6[shape6$pos == 5,]$x, y = shape6[shape6$pos == 5,]$y),
               fill = "springgreen4",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape6[shape6$pos == 5,]$x, y = shape6[shape6$pos == 5,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape6[shape6$pos == 4,]$x, y = shape6[shape6$pos == 4,]$y),
               fill = "springgreen4",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape6[shape6$pos == 4,]$x, y = shape6[shape6$pos == 4,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape6[shape6$pos == 3,]$x, y = shape6[shape6$pos == 3,]$y),
               fill = "springgreen4",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape6[shape6$pos == 3,]$x, y = shape6[shape6$pos == 3,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape6[shape6$pos == 2,]$x, y = shape6[shape6$pos == 2,]$y),
               fill = "springgreen4",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape6[shape6$pos == 2,]$x, y = shape6[shape6$pos == 2,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape6[shape6$pos == 1,]$x, y = shape6[shape6$pos == 1,]$y),
               fill = "springgreen4",
               color = "black",
               size = 1.5) +
  geom_polygon(aes(x = shape6[shape6$pos == 1,]$x, y = shape6[shape6$pos == 1,]$y),
               fill = "white",
               color = "white",
               size = 1.5) +
  geom_polygon(aes(x = shape6[shape6$pos == 0,]$x, y = shape6[shape6$pos == 0,]$y),
               fill = "springgreen4",
               color = "black",
               size = 1.5) +
  
  transition_layers(layer_length = 2, transition_length = 1) -> p

animate(p,
        nframes = 200,
        renderer = gifski_renderer("choinka_tetris.gif"),
        end_pause = 3)
