library(tidyverse)
library(ggplot2)

xValue <- c(18,22,22,18,18)
yValue <- c(-1,-1,4,4,-1)
points <- data.frame(xValue,yValue)

n <- 200 
tree <- tibble(x = runif(n, 6, 32),  
       y = runif(n, -2, 37),  
       s = runif(n, min = 60, max = 240)) %>%
  ggplot() +
  geom_point(aes(x, y, size = s), color = "white", pch = 42) +
  theme(panel.background = element_rect("black"))


tree <- tree + 
  geom_polygon(data = points, aes(x = xValue, y = yValue, fill = "#8c510a"))

xValue <- c(10,14,17,20,23,26,30,20)
yValue <- c(3,2,3,2,3,2,3,20)
points <- data.frame(xValue,yValue)

tree <- tree + geom_polygon(data = points, aes(x = xValue, y = yValue, fill = "#41ae76"))

xValue <- c(11,12,14,17,20,23,26,28,29,20)
yValue <- c(6,6,5,6,5,6,5,6,6,18)
points <- data.frame(xValue,yValue)
tree <- tree + 
  geom_polygon(data = points, aes(x = xValue, y = yValue, fill = "#006d2c"))

xValue <- c(12,13,15,17.5,20,22.5,25,27,28,20)
yValue <- c(9,9,8,9,8,9,8,9,9,20)
points <- data.frame(xValue,yValue)
tree <- tree + 
  geom_polygon(data = points, aes(x = xValue, y = yValue, fill = "#41ae76"))

xValue <- c(13,14,16,18,20,22,24,26,27,20)
yValue <- c(12,12,11,12,11,12,11,12,12,22)
points <- data.frame(xValue,yValue)
tree <- tree + 
  geom_polygon(data = points, aes(x = xValue, y = yValue, fill = "#006d2c"))

xValue <- c(14,15,16,18,20,22,24,25,26,20)
yValue <- c(15,15,14,15,14,15,14,15,15,24)
points <- data.frame(xValue,yValue)
tree <- tree + 
  geom_polygon(data = points, aes(x = xValue, y = yValue, fill = "#41ae76"))

xValue <- c(15,16,17,18.5,20,21.5,23,24,25,20)
yValue <- c(18,18,17,18,17,18,17,18,18,26)
points <- data.frame(xValue,yValue)
tree <- tree + 
  geom_polygon(data = points, aes(x = xValue, y = yValue, fill = "#006d2c"))

xValue <- c(16,17,18,19,20,21,22,23,24,20)
yValue <- c(21,21,20,21,20,21,20,21,21,28)
points <- data.frame(xValue,yValue)
tree <- tree + 
  geom_polygon(data = points, aes(x = xValue, y = yValue, fill = "#41ae76"))

xValue <- c(17,18,19,20,21,22,23,20)
yValue <- c(24,24,23,24,23,24,24,30)
points <- data.frame(xValue,yValue)
tree <- tree + 
  geom_polygon(data = points, aes(x = xValue, y = yValue, fill = "#006d2c"))

xValue <- c(18,19,20,21,22,20)
yValue <- c(27,27,26,27,27,32)
points <- data.frame(xValue,yValue)
tree <- tree + 
  geom_polygon(data = points, aes(x = xValue, y = yValue, fill = "#41ae76"))

xValue <- c(20,21.8,21.1,22.5,20.8,20,19.2,17.5,18.9,18.2,20)
yValue <- c(31.3,30,32.5,34,34,36,34,34,32.5,30,31.3)
yValue <- yValue-0.4
points <- data.frame(xValue,yValue)
tree <- tree + 
  geom_polygon(data = points, aes(x = xValue, y = yValue, fill = "yellow"))+
  scale_fill_identity() + 
  theme_bw() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") + 
  theme_void() +
  coord_cartesian(c(6,32), c(-2,37)) +
  theme(plot.background = element_rect(fill="black", colour="black"),
        panel.background = element_rect(fill="black")) 

tree
