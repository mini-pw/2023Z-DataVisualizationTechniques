library(dplyr)
library(plotly)
library(stringr)
library(tidyr)


df <- read.csv(file = "real_estate.csv")

df$Zillow.Estimate <- str_replace_all(df$Zillow.Estimate,",","")
df %>% 
  filter(yearBuilt != 0, !is.na(Zillow.Estimate)) %>% 
  mutate(Zillow.Estimate = as.numeric(Zillow.Estimate)) %>% 
  group_by(PropType, yearBuilt) %>% 
  summarise(srednia = mean(Zillow.Estimate)) %>% 
  pivot_wider(names_from = PropType,
              values_from = srednia,
              values_fill = 0) %>% 
  pivot_longer(cols = 2:5,
               names_to = "PropType",
               values_to = "srednia")-> df_clean

plot_ly(data = df_clean,
        x = ~PropType,
        y = ~srednia,
        frame = ~yearBuilt,
        type = "bar"
        ) %>% layout(
          title = "Mean price of houses by year",
          xaxis = list(title = "Type", range = c("Condominium", "MultiFamily2To4", "SingleFamily", "Townhouse")),
          yaxis = list(title = "Price", range = c(0,500000))
          ) %>% 
  animation_opts(400) %>%
  animation_button(x = 0.05, y = 0.05) %>%
  animation_slider(currentvalue = list(prefix = "Year: ", font = list(color="green")))

          

