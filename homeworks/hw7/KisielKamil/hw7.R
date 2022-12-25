library(tidyverse, quietly = T)
library(gganimate, quietly = T)

cone = data.frame(x = 1:9,
                  y = c(1:5,4:1)) %>% 
  na.omit() %>% 
  arrange(x)

fancy = cone %>% 
  mutate(xoff = ifelse(x<5, x+.4, ifelse(x>5, x-.4, NA))) %>% 
  gather(del, x, contains("x")) %>% 
  mutate(y = ifelse(del=="xoff", y-.1,y)) %>% 
  filter(y>=1) %>% 
  na.omit() %>% 
  select(-del) %>% 
  arrange(y)

bauble_colours = c("#e5d08f", "#e3d4b6",
                   "#cacbce", "#9c9c9c", "#e1dfdf",
                   "#c94c4c", "#8d1414")

baubles = cone %>% 
  group_by(y) %>% 
  nest() %>% 
  mutate(data =  map(data, ~data.frame(x=seq(min(.$x), max(.$x), by=.1)))) %>% 
  unnest(cols = c(data)) %>% 
  group_by(x) %>% 
  nest() %>% 
  mutate(data =  map(data, ~data.frame(y=seq(min(.$y), max(.$y), by=.1)))) %>% 
  unnest(cols = c(data)) %>% 
  ungroup() %>% 
  mutate(col1 = sample(bauble_colours, nrow(.), replace = T),
         col2 = sample(bauble_colours, nrow(.), replace = T),
         shp = sample(1:7, nrow(.), replace = T),
         sz = sample(seq(.5,2,by=.1), nrow(.), replace = T),
         time = sample(seq(.5,1,by=.01), nrow(.), replace = T)
  ) %>%
  rownames_to_column() %>% 
  sample_n(60) %>% 
  gather(dd, cols, contains("col")) %>% 
  mutate(alph = ifelse(dd == "col1", .8, 1))

cone %>% 
  ggplot(aes(x=x, y=y)) +
  geom_polygon(fill="#213c18") +
  geom_polygon(data=fancy, fill = "#668c6f") +
  geom_point(data = baubles %>% select(-time), show.legend = F, alpha = .7,
             aes(colour=I(cols), fill=I(cols),
                 shape = factor(shp),size=sz, group=rowname)) +
  geom_point(data = baubles, show.legend = F,
             aes(colour=I(cols), fill=I(cols), alpha=I(alph),
                 shape = factor(shp),size=sz, group=rowname)) +
  geom_point(data=data.frame(x=5, y=5), colour="#e5d08f", size=15, shape=8) +
  scale_shape_manual(values = c(20:25,8)) +
  theme_void() + 
  theme(plot.background = element_rect(fill="black"), title = element_text(colour="white"))