library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggimage)
library(ggExtra)

df <- read.csv("athlete_events.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")
country_code <- read.csv("country-codes_csv.csv")


# kod do stworzenia mapy

country_code %>% 
  select(IOC, ISO3166.1.Alpha.3) -> tmp2

df %>% 
  select(NOC, Season, Medal) %>% 
  filter(!is.na(Medal)) %>% 
  group_by(NOC, Season) %>% 
  summarise(medals = n()) %>% 
  pivot_wider(names_from = Season, values_from = medals) %>% 
  group_by(NOC) %>% 
  summarise(ratio_winsum = if_else(!is.na(Summer) & !is.na(Winter),
                            Summer/Winter,
                            if_else(is.na(Winter),
                                      max(Summer) + 1,
                                      -1))) %>% 
  mutate(sumwin = if_else(ratio_winsum > median(ratio_winsum),
                          "sum",
                          "win")) %>% 
  select(NOC, sumwin) %>% 
  left_join(tmp2, by = c("NOC" = "IOC")) -> tmp
                            
# Tutaj, gdy dane państwo ma jedynie medale w letnich to zwracam Inf, aby nie
# zaburzyć dzielenia, zimowe analogicznie tylko -Inf
world %>% 
  filter(sov_a3 != "ATA") %>% 
  left_join(tmp, by = c("brk_a3" = "ISO3166.1.Alpha.3")) %>%
ggplot(aes(fill = sumwin)) +
  geom_sf() +
  labs(title = "Wyniki poszczególnych państw świata na Igrzyskach Olimpijskich",
      subtitle = "według stosunku medali z igrzysk letnich do zimowych.") +
  scale_fill_discrete(name = "Lepsze wyniki podczas:",
                      labels = c("Letnich igrzysk", "Zimowych igrzysk", "Brak danych")) +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
     #transparent legend panel
    legend.key = element_blank()
  )
    
# wykres medale

img <- list.files(pattern = "medal-icon.png", full.names = TRUE)

country_code %>%
  select(IOC, Region.Name) -> tmp3


fun <- function(x) {
  x %>% 
    filter(!is.na(Medal)) %>% 
    select(NOC) %>% 
    table() %>% 
    as.data.frame() %>% 
    left_join(tmp3, by = c("." = "IOC")) %>% 
    select(Region.Name, Freq) %>%
    group_by(Region.Name) %>% 
    summarise(medals = sum(Freq)) %>%
    filter(!is.na(Region.Name) & Region.Name != "") %>%
    arrange(desc(medals)) %>% 
    head(3)
}

all <- fun(df)

ggplot(all, aes(x = reorder(reorder(Region.Name, -medals), c(2, 1, 3)), y = medals)) +
  geom_col(fill = "goldenrod2", width = 1) +
  
  labs(x = "",
       y = "") +
  scale_x_discrete(labels = c("Ameryka Południowa\ni Północna", "Europa", "Azja")) +
  scale_y_continuous(limits = c(0, 25000), expand = c(0, 0)) +
  
  geom_image(aes(image = img), size = 0.098, nudge_y = -1800) +
  geom_text(aes(label = medals), nudge_y = 1000, size = 6) +
  theme_minimal() +
  removeGrid(y = FALSE)
  


