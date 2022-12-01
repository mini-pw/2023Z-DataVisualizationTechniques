
library("dplyr")
library("ggplot2")
library("svglite")


goals 

goals_adjusted <- goals %>% 
  filter(minute_stoppage == 0 & minute_regulation <= 90)

ggplot(goals_adjusted, aes(x = minute_regulation)) +
  geom_histogram(fill = "#00326f", alpha = 0.75, binwidth = 2) +
  scale_x_continuous(breaks = scales::breaks_width(10), expand = c(0,0.01))+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "Liczba bramek",
       x = "Minuta meczu") +
  theme_minimal() +
  theme(text = element_text(size = 18),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) -> my_plot


ggsave("strzelone_gole_niebiesko_po_poprawkach.svg", my_plot, width = 10, height = 7)