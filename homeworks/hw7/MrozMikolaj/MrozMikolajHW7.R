install.packages("ggplot2")
install.packages("tidyverse")
install.packages("mlbench")
install.packages("viridis")
install.packages("ggstar")
install.packages("devtools")
library(ggplot2)
library(tidyverse)
library(mlbench)
library(viridis)
library(ggstar)
library(png)
library(grid)
library(devtools)

set.seed(12345)
data <- mlbench.shapes(n=100000) 
plot(data)

img <- png::readPNG("background.png")
gpp <- rasterGrob(img, interpolate=TRUE)
gpp$width <- unit(1, "npc") 
gpp$height <- unit(1, "npc")

y <- c(0.8, 1, 1, 0.8)  
x <- c(00.8, 00.8, 1.2, 1.2)

trunk = as.data.frame(cbind(x, y))

plot(x, y)
polygon(x, y, col = 'brown')

tree <- data %>%
  as.data.frame() %>% 
  filter(classes == "3")

plot(tree$x.x4, tree$x.V2, col = "darkgreen", xlab = "x", ylab = "y")

p1 <- ggplot() + annotation_custom(gpp, xmin=-1, xmax=3, ymin=0.8, ymax=2.5) +
  geom_polygon(aes(x = x, y = y), data = trunk, fill = "brown") +
  geom_point(aes(x = x.x4, y = x.V2), shape = 5, data = tree, colour = "darkgreen")

p1
bombki <- data.frame()

for(i in seq(1.05,1.6,by=0.15)){
  add <- data.frame(runif(5,1,2),rep(i,5))
  bombki <- rbind(bombki, add)
}
names(bombki) = c("xc","yc")


bombki["xc"] = bombki["xc"]-0.5



swiatelka <- data.frame()

for(i in seq(1.05,1.6,by=0.15)){
  add <- data.frame(runif(5,1,2),rep(i,5))
  swiatelka <- rbind(swiatelka, add)
}
names(swiatelka) = c("xc","yc")

swiatelka["xc"] = swiatelka["xc"]-0.5

gwiazda = data.frame(xc = 1, yc = 2.02)

choinka <- p1 + 
  geom_point(data = bombki , aes(x = xc, y =yc), size = 5, color = "darkred")
choinka
choinka <- choinka + geom_point(data = swiatelka, aes(x = xc, y = yc), size = 3, color = 'yellow')
choinka

choinka <- choinka + geom_point(data = gwiazda, aes(x = xc, y = yc), shape="\u2605", size=30, color = "yellow") +
  theme(panel.background = element_blank()) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
  

choinka




