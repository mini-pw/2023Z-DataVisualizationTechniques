# Przygotowanie
library(ggplot2)
library(plotly)
library(dplyr)
data <- read.csv("Properties_philly_Kraggle_v2.csv")

# Agregacja danych
df3 <- data %>%
  filter(PropType != "MultiFamily2To4") %>% #multifamily były budowane tylko na początku wieku
  filter(yearBuilt >= 1900 & yearBuilt <2000) %>%
  mutate(decade = gsub(" ", "", paste(as.character(floor(yearBuilt/10)*10),'s'))) %>%
  group_by(PropType, decade) %>%
  summarise(mean_sqft = mean(finished...SqFt.))

# Dodanie niektorych brakujących wartości
df3[nrow(df3) + 1, ] = list("Condominium", "1990s", 0)
df3[nrow(df3) + 1, ] = list("Townhouse", "1980s", 0)
df3[nrow(df3) + 1, ] = list("Townhouse", "1990s", 0)

# Wykres
figure <- plot_ly(data = df3,
               x = ~PropType,
               y = ~mean_sqft,
               frame = ~decade,
               type = "bar") %>%
      layout(
        yaxis = list(title = "Metraż (sqft)"),
        xaxis = list(title = "Rodzaj domu"),
        title = "Średni metraż w zależności od rodzaju domu w danej dekadzie XX wieku") %>% 
        animation_opts(frame = 500,
                       redraw = FALSE,
                       transition = 100) %>% 
        animation_button(label = "Odtwórz") %>%
        animation_slider(currentvalue = list(prefix = "Dekada: "))

figure

htmlwidgets::saveWidget(figure, "figure_hw4.html", selfcontained = F)
       

