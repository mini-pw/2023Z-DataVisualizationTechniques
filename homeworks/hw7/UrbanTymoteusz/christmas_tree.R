library(dplyr)
library(leaflet)
library(htmlwidgets)

# Początkowe współrzędne

rb1 = c(52.220100, 21.011760)
lb1 = c(52.220100, 21.005210)
rt1 = c(52.220400, 21.011760)
lt1 = c(52.220400, 21.005210)

delta_y = 0.000150
delta_x = 0.000200

# Choinka

tree = data.frame(rbind(rb1, lb1))
colnames(tree) <- c('y', 'x')

for (i in 1:16) {
  y = runif(1000%/%((i+1)%/%2), rb1[1], rt1[1])
  x = runif(1000%/%((i+1)%/%2), lb1[2], rb1[2])
  yx = cbind(y, x)
  tree = rbind(tree, yx)
  rb1[1] = rb1[1] + delta_y
  rt1[1] = rt1[1] + delta_y
  lb1[2] = lb1[2] + delta_x
  rb1[2] = rb1[2] - delta_x
}

m <- leaflet(tree) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~x,
                   lat = ~y,
                   radius = 2,
                   color = 'green')

# Pień

rb2 = c(52.219700, 21.008800)
lb2 = c(52.219700, 21.008000)
rt2 = c(52.220100, 21.008800)
tree2 = data.frame(rbind(rb2, lb2))
colnames(tree) <- c('y2', 'x2')
y2 = runif(100, rb2[1], rt2[1])
x2 = runif(100, lb2[2], rb2[2])
yx2 = cbind(y2, x2)
tree2 = rbind(tree2, yx2)

m <- m %>% addCircleMarkers(lng = x2,
                            lat = y2, 
                            radius = 5,
                            color = 'brown'
)

# Bombki

b1 = c(52.220573, 21.006843)
b2 = c(52.220495, 21.009593)
b3 = c(52.221026, 21.009094)
b4 = c(52.221661, 21.007616)
b5 = c(52.222082, 21.008754)
tree3 = data.frame(rbind(b1, b2, b3, b4, b5))
colnames(tree3) <- c('y3', 'x3')

m <- m %>% addCircleMarkers(lng = tree3$x3,
                            lat = tree3$y3, 
                            radius = 8,
                            color = 'blue',
                            opacity = 0.8,
                            fillOpacity = 0.6
)

b6 = c(52.220916, 21.010037)
b7 = c(52.221864, 21.008004)
b8 = c(52.220996, 21.006945)
b9 = c(52.221289, 21.008258)
b10 = c(52.220515, 21.007662)
tree4 = data.frame(rbind(b6, b7, b8, b9, b10))
colnames(tree4) <- c('y4', 'x4')

m <- m %>% addCircleMarkers(lng = tree4$x4,
                            lat = tree4$y4, 
                            radius = 8,
                            color = 'orange',
                            opacity = 0.8,
                            fillOpacity = 0.6
)

# Gwiazdka

m <- m %>% addCircleMarkers(lng = 21.008487,
                            lat = 52.222792, 
                            radius = 25,
                            color = 'yellow',
                            opacity = 0.8,
                            fillOpacity = 0.6
)
m

saveWidget(m, file="christmas_tree.html", selfcontained = TRUE)
