install.packages("ggimage")
install.packages("ggthemes")
install.packages("remotes")
remotes::install_github('rensa/ggflags')

library(dplyr)
library(tidyverse)
library(readxl)
library(ggimage)
library(ggthemes)
library(ggflags)
library(remotes)

windowsFonts(hel=windowsFont("Sans"))
chocolateCon <- read_xlsx("C:\\Users\\zareb\\OneDrive\\Desktop\\Studies\\3 semestr\\R\\projekt1\\czekolada\\chocolatePC.xlsx") 
chocolateCon$ChocolatePC <- substr(chocolateCon$ChocolatePC,1, 3)
chocolateCon <- chocolateCon %>%
  mutate(ChocolatePC = as.numeric(ChocolatePC))

medalsCount <- read_xlsx("C:\\Users\\zareb\\OneDrive\\Desktop\\Studies\\3 semestr\\R\\projekt1\\czekolada\\datamedals.xlsx") %>%
  rename(Country = NOC)
medalsCount$Country <- substr(medalsCount$Country, 2, nchar(medalsCount$Country))

population2020 <- read.csv("C:\\Users\\zareb\\OneDrive\\Desktop\\Studies\\3 semestr\\R\\projekt1\\czekolada\\population.csv") %>%
  select(name, pop2020) %>%
  mutate(pop2020 = pop2020 * 1000) %>%
  rename(Country = name) %>%
  mutate(Country = replace(Country, Country == "Japan", "Japan*")) %>%
  mutate(Country = replace(Country, Country == "United Kingdom", "Great Britain"))

merged <- left_join(chocolateCon, medalsCount, by = "Country")  

mergedall <- left_join(merged, population2020, by = "Country") %>%
  mutate(medalsPer10 = (Total/pop2020)*10000000) %>%
  mutate(goldPer10 = (Gold/pop2020)*10000000) %>%
  mutate(silverPer10 = (Silver/pop2020)*10000000) %>%
  mutate(bronzePer10 = (Bronze/pop2020)*10000000) 

mergedall %>%
  ggplot(aes(y=medalsPer10, x=ChocolatePC)) +
  geom_point(size = 0) +
  ggtitle("The dependence of chocolate on medals") +
  xlab("Annual consumption of chocolate per capita in kilograms") +
  ylab("Number of medals per 10mln inhabitants") + 
  geom_image(aes(image=flags), size=.061, asp = 2) +
  theme_tufte() +
  theme(plot.title = element_text(size = 34, face = "plain", family = "hel", hjust = 0.5), 
        panel.background = element_rect(fill = 'white', color = 'white'),
        axis.text=element_text(size=13),
        axis.title.x = element_text(size = 22, family = "hel"),
        axis.title.y = element_text(size = 22, family = "hel"),
        axis.text.x = element_text(size = 18, family = "hel"),
        axis.text.y = element_text(size = 18, family = "hel"),
        axis.ticks = element_blank()) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE) +
  coord_cartesian(ylim = c(0, 23))

ggsave("czekolada.png",
       width = 11,
       height = 7,
       dpi = 1000)





