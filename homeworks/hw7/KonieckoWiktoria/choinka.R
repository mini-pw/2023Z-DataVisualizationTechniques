library(ggplot2)
library(dplyr)
df <- read.csv("data.csv")
names(df) <- 1:21
df$row <- 1:19
dfcolours <- c("."="#1A1494", "&"="#FFFFFF",
               "|"="#815822", "#"="#00FF19", "*"="#E1FF00")
tidyr::pivot_longer(df, cols=1:21) %>% 
    mutate(name = as.numeric(name)) -> df
df %>% 
    ggplot(aes(x = name, y=row, fill=value)) + 
    scale_fill_manual(values=dfcolours) +
    scale_y_reverse(expand=c(0,0)) +
    scale_x_discrete(expand = c(0,0)) +
    geom_tile() +
    labs(y = NULL, x=NULL) +
    theme_void()+
    theme(legend.position = "none")
ggsave("choinka.png", device = "png", width = 42, height = 38, units = "mm")
