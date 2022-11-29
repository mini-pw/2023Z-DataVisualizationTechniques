setwd("C:/Users/rafp2/OneDrive/Pulpit/RAFA£/studia/sem3/TWD/project")

all_names <- read.csv("all_names.csv")
all_comps <- read.csv("all_comps.csv")
all_ratings <- read.csv("all_ratings.csv")
all_results <- read.csv("all_results.csv")
all_stats <- read.csv("all_stats.csv")
improved_names <- read.csv("improved_names.csv")

library(dplyr)
library(ggplot2)

df5<-merge(all_comps, all_results, by='id') 

df5 %>% 
  filter(gender != "Mix") %>% 
  ggplot(aes(x=speed, fill=gender)) +
  geom_density(alpha=0.4) +
  scale_fill_manual(values = c("#0a71a7","#a3e0ea")) +
  scale_x_continuous(expand = expansion(0, 0), limits = c(74, 110)) +
  theme_minimal() +
  labs(title = "Take-off speed: men vs women",
       x = "Take-off speed (km/h)",
       y = "Density"
  ) +
  theme(legend.position = c(0.8, 0.775),
        plot.title = element_text(size = 20, face = "bold")) +
  guides(fill=guide_legend(title="Gender"))