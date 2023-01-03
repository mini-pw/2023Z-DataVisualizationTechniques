library(dplyr)
library(ggplot2)
library(plotly)
okrag <- function(r,h){
  r_kwadrat <- r^2
  x <- seq(-r,r,1/10)
  y <- c(sqrt(abs(r_kwadrat-x^2)), -sqrt(abs(r_kwadrat-x^2)))
  x <- rep(x,2)
  h <- rep(h, length(x))
  df <- cbind(as.data.frame(y),as.data.frame(x),as.data.frame(h))
  df
}
o <- rbind(okrag(0,10))

for (i in 1:20) {
  r <- 0 + i*0.27
  h <- 10 - i*0.01
   o <- rbind(o,okrag(r, h))
}

for (i in 1:14) {
  r <- 2.5 + i*0.35
  h <- 9.8 - i*0.01
  o <- rbind(o,okrag(r, h))
}

for (i in 1:14) {
  r <- 4 + i*0.4
  h <- 9.66 - i*0.01
  o <- rbind(o,okrag(r, h))
}

for (i in 1:10) {
  r <- 1
  h <- 9.52 - i*0.01
  o <- rbind(o,okrag(r, h))
}
set.seed(1)
color <- c(sample(0:3, size = 9140, replace = T, prob = c(0.6,0.6,0.6,98.2)), rep(4,420))
df <- cbind(o,as.data.frame(color))

t1 <- list(
  family = "Times New Roman",
  color = "#000030"
)

df$color <- as.factor(df$color)
plot_ly(
  data = df, 
  x = ~x, 
  y = ~y, 
  z = ~h,
  color =  ~ color,
  colors= c("red", "blue", "yellow", "#0b9444", "#603813"),
  showlegend = F) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = "",
                                   backgroundcolor="#000030",
                                   gridcolor="#000030",
                                   showbackground=TRUE),
                      yaxis = list(title = "",
                                   backgroundcolor="#000030",
                                   gridcolor="#000030",
                                   showbackground=TRUE),
                      zaxis = list(title = "",
                                   backgroundcolor="#000030",
                                   gridcolor="#000030",
                                   showbackground=TRUE)
  ),
  font=t1,
  paper_bgcolor = "#000030"
  )

