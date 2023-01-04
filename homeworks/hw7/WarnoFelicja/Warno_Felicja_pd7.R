library(rbokeh)
library(dplyr)


plot = figure(xgrid = FALSE, ygrid = FALSE, xaxes = FALSE, yaxes = FALSE) %>%
  ly_lines(x = c(0,0), 
           y = c(-1,0.3),
           width = 20,
           color = "#6F4E37",alpha = 1
  )  %>%
  ly_lines(x = runif(200, -2, 2) * seq(0, 1, length = 200), 
           y = seq(10, 0, length = 200),
           width = 10,
           color = "#097969") %>%
  theme_plot(
    background_fill_color = '#00004d',
    
  ) %>% 
ly_points(x = runif(50,-2, 2), 
          y = runif(50,0, 10),
          size = 5,
          color = "white") %>% 
  ly_points(x = c(0, 0), 
            y = seq(10, 0, length = 1),
            size = 20,
            color = "#E1C16E")

plot

