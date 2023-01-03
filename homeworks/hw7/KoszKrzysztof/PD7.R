library(ggplot2)
library(RColorBrewer)

choinka<-data.frame(x = c(-10, 10, 4, 8, 3, 6, 0, -6, -3, -8, -4, -10, 
                         -1.5, 1.5, 1.5, -1.5),
                   y = c(2, 2, 6, 6, 10, 10, 14, 10, 10, 6, 6, 2, 0, 0, 2, 2),
                   part = rep(c("branches", "trunk"), times = c(12, 4))) 

bombki<-data.frame(x = c(-3.8, -4.8,  1.0, -0.6, -0.4, -2.6,  1.0,  2.4, -4.4,
                         -2.0,  3.4, -2.8, -2.8,  0.8,  4.2,  0.8, -1.6, -6.6,
                         1.0, -4.4, -0.2, -3.0,  4.0,  7.8,  2.6, -3.4,  7.4,
                         5.6,  2.0, -0.2,  7.6, -5.8, -3.8, -3.4, -5.2, -4.6,
                         1.8,  2.0, -0.8,  3.0,  3.6, -1.0, -2.8, -0.8, -1.0,
                         -1.8, -3.4,  1.4,  3.2,  2.4, -0.8,  2.0,  1.6,  4.6,
                         -5.0, -4.0, -5.8, -2.8, -2.2,  0.4),
                      y = c(6.0,  6.6,  2.4, 8.8, 10.2, 10.4,  2.2, 12.0,  3.0,
                            4.8,  2.4, 10.8,  4.4,  6.8,  6.8,  7.6,  6.2,  2.4,
                            8.6,  3.2,  4.8, 10.8,  9.0,  2.2,  2.6, 10.0,  3.0,
                            3.8,  3.4, 10.8,  2.6,  2.2,  4.4,  8.0,  3.6,  4.0,
                            9.2,  2.2, 11.8,  8.8,  4.0,  3.0,  4.0,  2.4, 10.6,
                            7.2,  7.0,  9.0, 11.6,  6.0,  5.4, 10.6,  6.2,  3.4,
                            3.2,  5.6,  7.2,  4.4,  5.6,  3.4),
                      color = sample(c(1,2,3,4,5), 60, TRUE))

ggplot(choinka, aes(x, y)) + 
  geom_polygon(aes(fill = part)) +
  geom_point(data = bombki, aes(color = color), size = 5) +
  scale_fill_manual(values = c("green4", "brown4")) +
  theme_minimal(base_size = 20)+
  geom_point(x = 0, y = 13.75, pch = 24, col = "gold", bg = "gold", cex = 8)+
  geom_point(x = 0, y = 13.75, pch = 25, col = "gold", bg = "gold", cex = 8)+
  theme(legend.position = "none")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(x="", y="")
