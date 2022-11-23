library(dplyr)
library(readxl)
library(ggplot2)
library(tidyverse)

#Tworzenie ramki danych
ore <- read_xlsx("C:\\Users\\zareb\\OneDrive\\Desktop\\Studies\\3 semestr\\R\\ZarebaAdrian\\2chart\\files\\ore.xlsx") %>%
  select(-c(Rank, Year)) %>%
  drop_na(Country) %>%
  rename(UIO = `Usable iron ore`) %>%
  filter(UIO != "2,500,000") 
ore$UIO <-  as.integer(ore$UIO) 

#Ukladanie wykresu
ore %>%
  ggplot(aes(x = UIO, y = reorder(Country, UIO))) +
  geom_col(fill = "orange") +
  geom_text(aes(label = UIO),
            hjust = 1, nudge_x = -.5) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,1000)) +
  theme(aspect.ratio=3/4) +
  xlab("Usable iron ore production (1000 tones)") +
  ylab(element_blank()) +
  ggtitle("List of countries by iron ore production")

