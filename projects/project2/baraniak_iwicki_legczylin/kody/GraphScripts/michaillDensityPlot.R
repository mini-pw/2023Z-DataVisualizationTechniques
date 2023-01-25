library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

lastname = "Legczylin"

df <- read_csv("./data/cleared/dane2.csv")

inGroup = F
counter = 1
startingCheck = T

for (i in 1:dim(df)[1]) {
  if (startingCheck == T) {
    df[i, "grId"] = 0
    
    if (df[i, "App.name"] == "Screen on (unlocked)") {
      startingCheck = F
    }
    
    next
  }
  
  # if in group
  if (inGroup == T) {
    # then mark it
    df[i, "grId"] = counter
  }
  
  # group started
  if (df[i, "App.name"] == "Screen off (locked)") {
    inGroup = T
  }
  
  # group ended
  if (df[i, "App.name"] == "Screen on (unlocked)") {
    inGroup = F
    counter = counter + 1
  }
}

print(df, n = 50)

df %>%
  filter(!is.na(grId)) %>%
  group_by(grId) %>%
  summarise(totalDuration = sum(as.integer(Duration))) %>%
  mutate(totalDurationInMinutes = round(totalDuration / 60, 1)) %>%
  mutate(
    DurMin = totalDurationInMinutes,
    totalDuration = NULL,
    totalDurationInMinutes = NULL
  ) %>%
  filter(DurMin <= 50) -> df_mod


ggplot(df_mod, aes(x = DurMin)) +
  geom_density(fill = "#123456f0") +
  theme(
    panel.background = element_blank(),
    legend.background = element_blank(),
    plot.background = element_blank()
  ) +
  labs(title = "How much time do we spent per session with phone?",
       x = "Time spent per session in minutes",
       y = "Frequency")
