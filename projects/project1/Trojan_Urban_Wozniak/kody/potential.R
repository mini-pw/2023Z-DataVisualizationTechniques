# przygotowanie
library(showtext)
library(dplyr)
library(ggplot2)
my_white <- rgb(255, 255, 255, 150, maxColorValue = 255)
font_add("knul", "C:/Users/tymek/Desktop/Studia/3_sem/TWD/project_fifa/data/Knul/Knul-Regular.otf")
showtext_auto()

fifa15 <- read.csv("players_15.csv")
fifa16 <- read.csv("players_16.csv")
fifa17 <- read.csv("players_17.csv")
fifa22 <- read.csv("players_22.csv")

# przygotowanie danych

pot15 <- fifa15 %>%
  select(sofifa_id, long_name, potential, overall) %>%
  filter(overall <= 71) %>%
  filter(potential >= 84)

pot16 <- fifa16 %>%
  select(sofifa_id, long_name, potential, overall) %>%
  filter(overall <= 71) %>%
  filter(potential >= 84)

pot17 <- fifa17 %>%
  select(sofifa_id, long_name, potential, overall) %>%
  filter(overall <= 71) %>%
  filter(potential >= 84)

pot <- rbind(pot15, pot16, pot17)
pot_g <- pot[!duplicated(pot$sofifa_id),]

ove22 <- fifa22 %>%
  mutate(overall22 = overall) %>%
  select(sofifa_id, overall22)
  
df <- merge(pot_g, ove22, by = "sofifa_id")
df %>%
  arrange(overall22)

# przygotowanie ramki do wykresu

df_s <- df %>%
  mutate(overalls = case_when(overall22 >= 90 ~ "90+",
                              overall22 >= 85 ~ "85-89",
                              overall22 >= 80 ~ "80-84",
                              overall22 >= 75 ~ "75-79",
                              overall22 >= 70 ~ "70-74",
                              overall22 >= 65 ~ "65-69",
                              overall22 >= 60 ~ "60-64",
                              TRUE ~ "59-")) %>%
  group_by(overalls) %>%
  summarise(count = n()) %>%
  mutate(percent = 100 * count / dim(pot_g)[1])

# dodanie brakujących wierszy

n_bad = dim(pot_g)[1] - dim(df)[1]
overalls <- c(".N/A", "85-89")
count <- c(n_bad, 0)
percent <- c(100 * n_bad / dim(pot_g)[1], 0)
df_tmp <- data.frame(overalls, count, percent)
df_f <- rbind(df_tmp, df_s)

# wykres

pot_plot <- ggplot(df_f, aes(x = overalls, y = percent)) +
  geom_col(fill = '#ccf53f') +
  labs(x = "Rating range", y = "Percentage") +
  ylim(0, 40) +
  theme(
    rect = element_rect(fill = 'transparent'),
    text = element_text(family = 'knul'),
    panel.background = element_rect(fill = 'transparent'),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(size = 0.2, color = my_white),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 25, vjust = 6, color = 'white'),
    axis.text.y = element_text(size = 25, color = 'white'),
    axis.title = element_text(size = 30, color = 'white'),
    axis.title.x = element_text(vjust = 4),
    axis.ticks = element_blank()
  ) 

pot_plot

ggsave(plot = pot_plot, file = "potential.png", 
       type = "cairo-png",  bg = "transparent",
       width = 1400, height = 800, units = 'px')


