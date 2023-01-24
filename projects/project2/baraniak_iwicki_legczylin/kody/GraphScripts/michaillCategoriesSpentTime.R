library(readr)
library(dplyr)
library(ggplot2)

o_df <- read_csv("./Data/Legczylin/data_tel.csv")

o_df %>%
  mutate("...1" = NULL) %>%
  filter(type != "Ekran" & type != "Urzadzenie") -> o_df

o_df %>%
  group_by(type) %>%
  summarise(total = sum(duration)) %>%
  mutate(total = round(total / 3600, 2)) -> df

ggplot(df, aes(x = type, y = total)) +
  geom_col(fill = "#63524195") +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank()
  ) +
  labs(title = "Total time spent per category",
       x = "Categories",
       y = "Time in hours")