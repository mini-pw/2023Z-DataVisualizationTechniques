library(dplyr)
library(ggplot2)

plot_cat <- function(df){
df %>%
  mutate(percentage = count/sum(count)*100) %>%
  mutate(category = case_when(
    percentage <= 5 ~ "Inne",
    category == "Music" ~ "Muzyka",
    category == "Entertainment" ~ "Rozrywka",
    category == "Film & Animation" ~ "Film i animacja",
    category == "Gaming" ~ "Gry",
    category == "People & Blogs" ~ "Ludzie i blogi",
    category == "Education" ~ "Edukacja",
    category == "Comedy" ~ "Komedia",
    category == "Comedy" ~ "Komedia",
    category == "Howto & Style" ~ "Projektowanie",
    TRUE ~ category
    )) %>%
  group_by(category) %>%
  summarize(percentage = sum(percentage)) %>%
  arrange(-percentage) %>%
  ggplot(aes(y = reorder(category, percentage), x = percentage)) +
  geom_col(fill = "#fb0404") +
  theme_minimal() + 
  labs(y = "") +
  theme(panel.background = element_rect(fill = "#100c0c"),
        plot.background = element_rect(fill = "#100c0c"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray34"),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(color = "white")) -> p 

ggplotly(p, tooltip = "text") %>% 
  layout(hoverlabel = list(bgcolor = "#2e2d2d"),
         xaxis = list(title = "Procent oglądanych kategorii [%]", color = "white",
                      titlefont = list(size = 20), tickfont = list(size = 15)),
         yaxis = list(tickfont = list(size = 15))) -> p
p
}



