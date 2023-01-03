library(ggplot2)

c_density <- function(x, lambda = 1) {
  1 / ((1 + (x / lambda) ** 2) * lambda * pi)
}

add_to_tree <- function(plot, x, y) {
  plot + polygon(all_x, all_y, col = "darkgreen", border = "darkgreen")
}


##### bazowy wykres
plot(c(-2, 2), c(0, 0.6), col = "white",type="n",xlab="",ylab="",xaxt="n",yaxt="n") -> plot

n <- 10000

##### pień

all_x <- c(-0.3, 0.3, 0.2, -0.2)
all_y <- c(0.05, 0.05, 0.2, 0.2)

plot + polygon(all_x, all_y, col = "brown", border = "brown") -> plot


##### dolny człon
x <- seq(from = -2, to = 2, length.out = n)
y <- c_density(x, 0.7)
y_bottom <- c_density(x, 2) - 0.03

all_x <- c(x, rev(x))
all_y <- c(y, rev(y_bottom))

plot <- add_to_tree(plot, all_x, all_y)


##### środkowy człon
# skalowanie
all_x <- all_x * 0.7
all_y <- all_y * 0.7 + 0.2

plot <- add_to_tree(plot, all_x, all_y)

##### górny człon
# skalowanie
all_x <- all_x * 0.7
all_y <- all_y * 0.7 + 0.2

plot <- add_to_tree(plot, all_x, all_y)


##### gwiazda
points(x = 0, y = max(all_y) + 0.01, pch = 24, col = "gold", bg = "gold", cex = 5)
points(x = 0, y = max(all_y) + 0.01, pch = 25, col = "gold", bg = "gold", cex = 5)


##### bombki
x <- c(-1.2, -0.8, -0.8, -0.76, -0.65, -0.44, -0.39, -0.25, -0.18, -0.11,
       0.1, 0.15, 0.22, 0.31, 0.38, 0.47, 0.56, 0.61, 0.78, 1)
y <- c(0.09, 0.12, 0.26, 0.36, 0.19, 0.15, 0.29, 0.2, 0.44, 0.36,
       0.46, 0.28, 0.18, 0.37, 0.24, 0.4, 0.17, 0.3, 0.36, 0.11)

points(x = x, y = y, pch = 21, col = "red", bg = "red", cex = 3)

