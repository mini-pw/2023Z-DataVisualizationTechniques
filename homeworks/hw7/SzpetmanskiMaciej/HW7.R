library(dplyr)
library(ggplot2)
library(ggstar)

x1 <- runif(20000,-5,5)
y1 <- (5-abs(x1))*runif(20000,0,1)
warstwa1 = data.frame(x = x1,y = y1)

x2 <- runif(4000,-4,4)
y2 <- (4-abs(x2))*runif(4000,0,1) + 1.5
warstwa2 = data.frame(x = x2,y = y2)

x3 <- runif(3000,-3,3)
y3 <- (3-abs(x3))*runif(3000,0,1) + 3
warstwa3 = data.frame(x = x3,y = y3)

x4 <- runif(2000,-2,2)
y4 <- (2-abs(x4))*runif(2000,0,1) + 4.5
warstwa4 = data.frame(x = x4,y = y4)

xb <- rep(c(0,-1.2, 1.2,-1.7,-0.2,1,-2,-0.5,0.7,2.7,-4),2)
yb <- rep(c(5,4.2, 4, 3,2.4, 2.8,1.5,1,1.2,1.8,0.6),2)
bombki <- data.frame(x=xb,y=yb) 
pien = data.frame(x = runif(1000,-0.5,0.5),y = runif(100,-0.5,0))

xs <- runif(2000,-6,6)
ys <- runif(2000,0,7)
mode <- 1:10
snieg <- data.frame(x=xs,y=ys,mode=mode)

p <- warstwa1 %>% ggplot(aes(x=x,y=y),main = "Title") + geom_point(colour = "green", shape = 17) +
  geom_point(data = warstwa2, color = "green",shape = 17) +
  geom_point(data = warstwa3, color = "green", shape = 17) +
  geom_point(data = warstwa4, color = "green", shape = 17) +
  geom_point(data = bombki, color = "#CD1504", size = 7) +
  geom_point(data = pien, color = "brown") +
  geom_star(data = data.frame(x = 0, y = 6.7), fill = "#FFD700", size = 15) + 
  geom_point(data = snieg, aes(group = seq_along(mode)), color = "white", shape = 8) +
  theme_void() + 
  theme(plot.background = element_rect(fill="cyan"))
  

p
