library(highcharter)
library(dplyr)
library(readr)
HW6_data <- read_csv("HW6_data.csv")
View(HW6_data)
names(HW6_data)<-c("After_0_matches","After_1_match","After_2_matches","After_3_matches")


HW6_data%>% 
  as.data.frame() %>% 
  make_long(After_0_matches,After_1_match,After_2_matches,After_3_matches) %>% 
  ggplot(aes(
    x = x, 
    next_x = next_x,
    node = node,
    next_node = next_node,
    fill = factor(node),
    label = paste0(node," pkt")
    )
  )+
  geom_sankey(
    flow.alpha = 0.5, 
    node.color = "black",
    show.legend = F
    )+
  geom_sankey_label(size = 2, color = "black", fill= "white", hjust = -0.1)+
  theme_bw() + 
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
    )+
  scale_fill_viridis_d(option = "inferno")+
  scale_x_discrete(labels=c(
    "After_0_matches"="Przed pierwszym meczem",
    "After_1_match" = "Po jednym meczu",
    "After_2_matches"="Po dwóch meczach",
    "After_3_matches"="Po trzech meczach"
    ))+
  labs(title = "Liczba punktów po danej liczbie meczy na Mistrzostwach Świata w Katarze",
       fill = 'Nodes')
