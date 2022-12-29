#HW7
#£ukasz Grabarski

library(dplyr)
library(ggplot2)
library(gganimate)
library(tidyr)

#choinka
x <- rnorm(3000)
y <- runif(length(x), min = 0, max = 3 - 2 * sign(x) * x)
choinka <- data.frame(x, y)

#snieg
xsnieg <- runif(1000,-4, 4)
ysnieg <- runif(length(xsnieg), 0.1, 100)

snieg <- data.frame(xsnieg,
                    '1' = ysnieg,
                    '2' = 0.80 * ysnieg,
                    '3' = 0.65 * ysnieg ) %>%
  pivot_longer(cols = c('X1', 'X2', 'X3')) %>%
  mutate(name = as.numeric(substr(name, 2, 2)))

psnieg <-  ggplot(snieg, aes(xsnieg, value)) +
  geom_point(aes(group = xsnieg), shape = 8, color = "white", size = 4)+
  theme_void()+
  theme(panel.background = element_rect(fill = 'black', color = 'black'),
        legend.position = "none")

#wykres
psnieg + transition_time(name) + 
  geom_point(data = choinka, aes(x, y, colour = y)) +
  scale_color_gradient(low = '#204227' , high = "#588a62") +
  geom_point(aes(x = 0, y = 2.8), shape = 24, size = 8, fill = "#d6c045", color = "#d6c045")+
  geom_point(aes(x = 0, y = 2.8), shape = 25, size = 8, fill = "#d6c045", color = "#d6c045")+
  scale_y_continuous(limits = c(0,3.5), expand = c(0,0)) +
  labs(x = NULL, y = NULL)
 