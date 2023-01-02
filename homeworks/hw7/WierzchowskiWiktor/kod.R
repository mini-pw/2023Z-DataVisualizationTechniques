library(plotly)

x1 = c(rep(seq(1, 7, 0.1),10))
z1 = c(rep(1, 61),rep(2, 61),rep(3, 61),rep(4, 61),rep(5, 61),rep(6, 61),
      rep(7, 61),rep(8, 61),rep(9, 61),rep(9.75, 61))

Fun <- function(x, z) {
  (sqrt((10-z) - (x-4)^2) + 4)
}

y1 = Fun(x, z)

df <- data.frame(
  x = x1,
  y = y1,
  z = z1,
  type = c(rep(c(rep('choin', 10), rep('niechoin', 1)), 55), rep('choin', 5))
)

p1 <- plot_ly(df, x = ~x1, y = ~y1, z = ~z1, color = ~type, colors = c('#165c29', '#ff0000'))
p1

x2 = c(rep(seq(1, 7, 0.1),10))
z2 = c(rep(1, 61),rep(2, 61),rep(3, 61),rep(4, 61),rep(5, 61),rep(6, 61),
      rep(7, 61),rep(8, 61),rep(9, 61),rep(9.75, 61))

Fun2 <- function(x, z) {
  (-sqrt((10-z) - (x-4)^2) + 4)
}

y2 = y = Fun2(x, z)

df2 <- data.frame(
  x = x2,
  y = y2,
  z = z2,
  type = c(rep(c(rep('choin', 10), rep('niechoin', 1)), 55), rep('choin', 5))
)

p2 <- plot_ly(df, x = ~x2, y = ~y2, z = ~z2, color = ~type, colors = c('#165c29', '#ff0000'))
p2

df3 <- data.frame(
  x = c(3.5, 4, 4, 4, 4.5, 4, 4),
  y = c(4  ,3.5, 4, 4.5,   4, 4, 4),
  z = c(10.5, 10.5, 10.5, 10.5, 10.5, 10, 11),
  type = c(rep('gwzdz', 7))
)


dfend <- rbind(df, df2, df3)
choin <- plot_ly(dfend, x = ~x, y = ~y, z = ~z, color = ~type, colors = c('#165c29', '#fbff00', '#ff0000'))



