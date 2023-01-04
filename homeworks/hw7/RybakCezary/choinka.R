library(dplyr)
library(ggplot2)
library(ggstar)
set.seed(100)
x = runif(30000, min=0, max=10)
y = runif(30000, min=0, max=10)
df <- data.frame(x, y)
xmasTree <- df %>% 
  filter((2*y < 3*x - 1 & 2*y < -3*x + 29 & y > 1) 
         | (y > 5.5 & 2*y < 3*x + 2 & 2*y < -3*x + 32) 
         | (y > 7.75 & 2*y < 3*x + 3.5 & 2*y < -3*x + 33.5)
         | (x > 4 & x < 6 & y < 1))
rhombusX = head(xmasTree$x, 10)
rhombusY = head(xmasTree$y, 10)
baublesX = tail(xmasTree$x, 10)
baublesY = tail(xmasTree$y, 10)
xmasTree <- xmasTree %>% 
  mutate(type = case_when(
    (x > 4 & x < 6 & y < 1) ~ "trunk",
    abs(y - 9.191104) < 0.00001 ~ "star",
    (y %in% rhombusY & x %in% rhombusX) ~ "rhombus",
    (y %in% baublesY & x %in% baublesX) ~ "bauble",
    TRUE ~ "tree"
  )) %>% 
  mutate(size = case_when(
    type == "star" ~ 8,
    type == "rhombus" ~ 6,
    type == "bauble" ~ 5,
  ))

decorations <- xmasTree %>% 
  filter(type %in% c("star", "rhombus", "bauble"))

tree <- xmasTree %>% 
  filter(type %in% c("tree", "trunk"))
  
plot <- ggplot() + 
  geom_point(data = tree, aes(x = x, y = y, color = type)) +
  scale_color_manual(values = c("darkgreen", "brown")) +
  geom_star(data = decorations, aes(x=x, y=y, starshape=type, fill = type) ,size=decorations$size) +
  scale_fill_manual(values=c("red", "orange", "yellow")) +
  scale_starshape_manual(values = c(15, 12, 1))
plot
