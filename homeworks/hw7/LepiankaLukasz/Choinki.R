library(ggplot2)
library(tidyr)
library(dplyr)

#Za każdym razem otrzymujemy nieco inną choinkę :)

#Jeśli nie rysuje tak jak powinno (wyświetla tylko zielone) to należy wyczyścić manualnie zmienne
rm(list = ls())

r1 <- rnorm(5000)
r2 <- rnorm(5000,0, 0.75)
r3 <- rnorm(5000,0, 0.5)
r4 <- rnorm(5000,0, 0.25)
r0 <- c(0)

con <- as.data.frame(cbind(r0,r1,r2,r3,r4))
con <- pivot_longer(con, cols = 1:5)

p <- ggplot(con, aes( x = value, y = name)) +
  geom_jitter(col = "darkgreen", alpha = 0.4) +
  geom_boxplot(color = "brown", fill = "darkgreen", outlier.alpha = 0, lwd = 1.5, alpha = 0.8)

r0 <- c()
r1 <- runif(3, -2, 2)
r2 <- runif(3, -1.5, 1.5)
r3 <- runif(3,-1, 1)
r4 <- runif(3,-0.5, 0.5)

df <- as.data.frame(cbind(r1,r2,r3,r4))
df <- pivot_longer(df, cols = 1:4) 

p <- p + geom_point(data = df, col = "blue", size = 6) 

r0 <- c()
r1 <- runif(5, -2, 2)
r2 <- runif(5, -1.5, 1.5)
r3 <- runif(5,-1, 1)
r4 <- runif(5,-0.5, 0.5)

df <- as.data.frame(cbind(r1,r2,r3,r4))
df <- pivot_longer(df, cols = 1:4) 

p <- p + geom_point(data = df, col = "yellow", size = 3) 
p <- p + theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_rect(fill = "lightblue"),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())
p
