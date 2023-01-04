library(dplyr)
library(ggplot2)
library(gganimate)

frames <- 1:24
frame <- NA
x <- NA
y <- NA
group <- NA
DENSITY <- 60
R <- 0.25

for (j in frames){
  
  x <- c(x, 0)
  y <- c(y, 7)
  frame <- c(frame, j)
  group <- c(group, "tree")
  
  for(i in seq(0, 2, 0.1)) {
    
    d <- seq(-i/2, i/2, 0.01)
    new_x <- sample(d, i * DENSITY * 0.8)
    new_y <- rep(7 - i, i * DENSITY * 0.8)
    new_frame <- rep(j, i * DENSITY * 0.8)
    new_group <- rep("tree", i * DENSITY * 0.8)
    
    x <- c(x, new_x)
    y <- c(y, new_y)
    frame <- c(frame, new_frame)
    group <- c(group, new_group)
    
  }
  
  for(i in seq(0, 2.5, 0.1)) {
    
    d <- seq(-i * 0.6, i * 0.6, 0.01)
    new_x <- sample(d, i * DENSITY)
    new_y <- rep(5 - i, i * DENSITY)
    new_frame <- rep(j, i * DENSITY)
    new_group <- rep("tree", i * DENSITY)
    
    x <- c(x, new_x)
    y <- c(y, new_y)
    frame <- c(frame, new_frame)
    group <- c(group, new_group)
    
  }
  
  for(i in seq(0, 2.5, 0.1)) {
    
    d <- seq(-3*i/4, 3*i/4, 0.01)
    new_x <- sample(d, i * DENSITY * 1.2)
    new_y <- rep(2.5 - i, i * DENSITY * 1.2)
    new_frame <- rep(j, i * DENSITY * 1.2)
    new_group <- rep("tree", i * DENSITY * 1.2)
    
    x <- c(x, new_x)
    y <- c(y, new_y)
    frame <- c(frame, new_frame)
    group <- c(group, new_group)
    
  }
  
  for (i in seq(0.1, 0.5, 0.1)) {
    
    d <- seq(-0.25, 0.25, 0.01)
    new_x <- sample(d, DENSITY / 2)
    new_y <- rep(-i, DENSITY / 2)
    new_frame <- rep(j, DENSITY / 2)
    new_group <- rep("bottom", DENSITY / 2)
    
    x <- c(x, new_x)
    y <- c(y, new_y)
    frame <- c(frame, new_frame)
    group <- c(group, new_group)
    
  }
  
  for (i in seq(1, 3, 2)) {
    
    angle <- i * 45
    x_range <- R * cos(angle * pi / 180)
    d <- seq(-abs(x_range), abs(x_range), 0.01)
    new_x <- sample(d, 20, replace = TRUE)
    new_y <- 7 + new_x * tan(angle * pi / 180)
    new_frame <- rep(j, 20)
    new_group <- rep("star", 20)
    
    x <- c(x, new_x)
    y <- c(y, new_y)
    frame <- c(frame, new_frame)
    group <- c(group, new_group)
    
  }
  
  x <- c(x, rep(0, 20))
  y <- c(y, sample(seq(7 - R, 7 + R, 0.01), 20))
  frame <- c(frame, rep(j, 20))
  group <- c(group, rep("star", 20))
  
  x <- c(x, sample(seq(-R, R, 0.01), 20))
  y <- c(y, rep(7, 20))
  frame <- c(frame, rep(j, 20))
  group <- c(group, rep("star", 20))
  
}

x <- na.omit(x)
y <- na.omit(y)
frame <- na.omit(frame)
group <- na.omit(group)

xdf <- data.frame(x, y, frame, group)

x_point <- ggplot(xdf, aes(x = x, y = y, color = group)) + geom_point() + scale_color_manual(values = c("tree" = '#2eb853', "bottom" = "#914507", star = "#e4ff61")) 

x_anim <- x_point + 
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none") +
  transition_states(frame,
                    transition_length = 2,
                    state_length = 1)
animate(x_anim, nframe = 24)

anim_save("animation.gif")
