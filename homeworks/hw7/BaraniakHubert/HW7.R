x1 <- sort(runif(20) + 1)
x2 <- sort(runif(20) + 1)
x3 <- sort(runif(20) + 1)
x4 <- sort(runif(20) + 1)
x5 <- sort(runif(20) + 1)
x <- c(x1, x2, x3, x4, x5)
x <- c(x, x*1.2, x*1.4)
y <- c(1/x1, 2/x2 - 0.5, 3/x3 - 1, 4/x4 - 1.5, 5/x5 - 2)
y <- c(y, y-1.5, y - 3)
s <- c(1:20, 1:20, 1:20, 1:20, 1:20)
s <- c(s, s, s)
t <- c(rep(0, 20), rep(1, 20), rep(2, 20), rep(3, 20), rep(4, 20))
t <- c(t,t + 5, t+10)

x <- x - 1
x <- c(x, -x)
y <- c(y, y)
s <- c(s, s)
t <- c(t, t + 15)

R_site <- data.frame(x, y, s, t)
ggplot(R_site, aes(x, y)) +
  geom_line(aes(group = t), colour = "darkgreen") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), 
                              axis.line = element_blank()) + 
  gganimate::transition_reveal(s)
