library(dplyr)
library(ggplot2)
library(forcats)

all_comps <- mutate(all_comps, id = trimws(id)) #There are spaces at the end of each tournament ids, but there were no spaces in other dataframes
improved_names <- improved_names %>% #whitebars problems fixed
  mutate(improved_names, nationality = trimws(nationality),
         gender = trimws(gender),
         name = trimws(name))

real_comps <- filter(all_comps, training == 0)
great5 <- c("GER", "AUT", "SLO", "POL", "NOR")


national_distance <- merge(all_results, improved_names, by = 'codex') %>%
  select(gender, nationality, dist, id) %>%
  merge(select(real_comps, id), by = 'id') %>%
  filter(gender == "M") %>%
  select(nationality, dist) %>%
  group_by(nationality) %>%
  summarise(sum_dist = sum(dist), apps = n()) %>%
  filter(apps > 1000) %>%
  mutate(avg_national_jump = sum_dist / apps) %>%
  arrange(-sum_dist)


national_speed <- merge(all_results, improved_names, by = 'codex') %>%
  merge(all_comps, by = 'id') %>%
  filter(gender.x == 'M', k.point < 170, training == 0, speed > 0) %>% # excluding mammoths and trainings
  group_by(nationality) %>%
  summarise(national_apps = n(), speed, nationality) %>%
  filter(national_apps > 100)


national_speed %>%
  filter(nationality %in% c("GER", "AUT", "SLO", "POL", "NOR", "FRA", "CHN")) %>%
  ggplot(aes(x = speed, y = reorder(nationality, speed), fill = factor(..y..))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_manual(values = c("#0b6ca3", "#1480af", "#299fc3", "#4cbbd6", "#63c8dc", "#80d4e2", "#a3e0ea")) +
  xlim(74, 100) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  labs(title = "Distribution of take-off speed for countries",
       x = "Speed (km/h)",
       y = "Nationality") +
  theme(plot.title = element_text(size = 20, face = "bold"))


as.data.frame(national_distance) %>%
  mutate(Colour = if_else(nationality %in% great5, "Great 5", "The Rest")) %>%
  ggplot(mapping = aes(y = apps, x = reorder(nationality, -sum_dist), fill = Colour)) +
  geom_bar(stat = 'identity') +
  labs(title = "Number of jumps per country (trainings excluded)") +
  xlab("Countries") +
  ylab("") +
  scale_fill_manual(values = c("#0a71a7", "#a3e0ea")) +
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        legend.position = c(0.8, 0.775)) +
  scale_y_continuous(expand = c(0, 0))
