#TWD P1
#Part 1 - silly plots



library(ggplot2)
library(dplyr)
source("themes.R")
library(ggtextures)
library(magick)
library(graphics)
library(tidyverse)

infaltion <- read.csv2("inflation.csv")
tv <- read.csv("tv.txt")
merged <- read.csv("Merged.txt")

install.packages("remotes")
remotes::install_github("clauswilke/ggtextures")


#wykres liniiowy

tv %>% 
  mutate(year = 1966 + super_bowl) -> tv

inner_join(tv, infaltion,
           by = "year") -> tv

tv %>% 
  mutate(cpi = as.numeric(cpi)) %>% 
  mutate(real_ad_value = 294.4 / cpi * ad_cost) -> tv

options(scipen = 100)
ggplot(tv, aes(x = year, y = real_ad_value / avg_us_viewers, fill="black")) +
  labs(title = "Koszt reklamy na jednego widza 1967-2018",
       x = "rok",
       y = "koszt w dolarach") +
  geom_line(size=1.2, color = "#f7fcb9") +
  scale_y_continuous(limits = c(0, 0.06), expand = c(0,0)) +
  scale_x_continuous(limits = c(1967,2018), expand = c(0,0))+
  theme_dark_blue()+
  theme(panel.border = element_rect(colour = "#d3d5df", fill=NA, size=1.2))


# Teraz zamuje sie wykresem z reklamami, z piktogramami
ads <- read.csv("superbowl-ads.txt")
top_brands <- c("Bud Ligh
                t", "Budweiser", "Doritos", "Pepsi", "Hyundai", "Coca-Cola", "Kia", "NFL")
options(scipen = 999)

#tu sa moje ikony, ich nie wrzucam
images = list(
  image_read_svg(path = "./images/Untitled.svg"),
  image_read_svg(path = "./images/doritos.svg"),
  image_read_svg(path = "./images/pepsi.svg"),
  image_read_svg(path = "./images/cola.svg"),
  image_read_svg(path = "./images/kia.svg"),
  image_read_svg(path = "./images/nfl.svg")
)

ads %>% 
  filter(brand %in% top_brands) %>%
  mutate(brandx = fct_infreq(ifelse(brand =='Bud Light', 'Budweiser', brand))) -> ads


ads_to_plot<- data.frame(table(ads$brandx)) 
names(ads_to_plot) <- c("brandx", "count")

ads_to_plot$count[[1]] <- 102

ads_to_plot %>% 
  ggplot(aes(x = brandx, y = count, image = images)) +
  labs(title = "Najpopularniejsze marki reklamowane w Superbowl",
       x = "Marka",
       y = "Częstość") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 120)) +
  geom_col()+
  geom_isotype_col(
    img_height = grid::unit(1, "null"), img_width = NULL,
    ncol = 1, nrow = 1, hjust = 0.5, vjust = 0)+
  theme_dark_blue()

# kolumnowa ogl
tv %>%
  mutate(avg_us_viewers = ifelse(avg_us_viewers == 26750000, (26750000
                                                              +24430000), avg_us_viewers)) %>%
  filter(avg_us_viewers != 24430000) %>%
  mutate(year = super_bowl + 1966, views = avg_us_viewers/1000000) %>%
  ggplot(aes(x=year, y=views)) +
  labs(title = "Średnia oglądalność Superbowl w USA",
       x = "Rok",
       y = "Średnia liczba oglądających (w mln)")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 120)) +
  scale_x_discrete(limits = c(1967, 1980, 1990, 2000,2010, 2018)) +
  geom_col(fill='pink', color = "black") +
  theme_dark_blue()

