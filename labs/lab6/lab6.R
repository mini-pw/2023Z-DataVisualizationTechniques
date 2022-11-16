##########################################
###    TECHNIKI WIZUALIZACJI DANYCH    ###
###          LABORATORIUM  6           ###
##########################################


## https://plotly.com/r
## https://github.com/plotly/plotly.R/issues
library(plotly)
library(dplyr)


x <- c("a", "b", "c")
y <- c(1, 3, 2)

# ~ matplotlib / ggplot2
fig <- plot_ly() %>% 
  add_lines(x = x, y = y) %>% 
  layout(
    title = "sample figure", 
    xaxis = list(title = 'x'), 
    yaxis = list(title = 'y'), 
    plot_bgcolor = "#c7daec"
  ) 

# obiekt jak w ggplot2
str(fig$x) 

fig

plot_ly(x = x, y = y) # No trace type specified
   
plot_ly(x = x, y = y, type = 'type')

plot_ly(x = x, y = y, type = 'bar')

plot_ly(x = x, y = y, type = 'scatter') # No scatter mode specified

## https://plotly.com/r/reference/#scatter-mode
plot_ly(x = x, y = y, type = 'scatter', mode = 'lines')

# wykres statyczny
fig <- plot_ly() %>%
  add_trace(x = x, y = y, type = "bar")
config(fig, staticPlot = TRUE)

?config

# wykres bez zoom i przycisków
fig %>% 
  layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) %>%
  config(displayModeBar = FALSE)



# ~ seaborn / ggplot2
df_raw <- read.csv("https://raw.githubusercontent.com/mini-pw/2021Z-DataVisualizationTechniques/master/labs/data/Pokemon.csv")[,-1]

df <- df_raw %>%
  filter(Type.1 %in% c("Fire", "Water", "Grass", "Poison", "Electric")) %>%
  mutate(Type.1 = factor(Type.1, levels = c("Fire", "Water", "Grass", "Poison",  "Electric")))

plot_ly(
  data = df, 
  x = ~Attack, 
  y = ~Defense, 
  color = ~Type.1,
  colors = "Set1"
)

## 3D
plot_ly(
  data = df, 
  x = ~Attack, 
  y = ~Defense, 
  z = ~HP,
  color = ~Type.1, 
  colors = "Set1",
  type = "scatter3d",
  mode = "markers"
)

plot_ly(
  data = df, 
  x = ~Attack, 
  y = ~Defense, 
  z = ~Speed,
  color = ~Type.1, 
  colors = "Set1",
  symbol = ~Type.1, 
  symbols = c('circle', 'cross', 'diamond', 'square', 'circle'),
  marker = list(size = 5, line = list(color = 'black', width = 1)),
  type = "scatter3d",
  mode = "markers"
)

## subplots: https://plotly.com/r/mixed-subplots

## tooltip https://plotly.com/r/hover-text-and-formatting
## legend https://plotly.com/r/legend
plot_ly(
  data = df, 
  x = ~Attack, 
  y = ~Defense, 
  color = ~Legendary,
  colors = c("black", "red"),
  text = paste0("Name: ", df$Name, "<br>Stage: ", df$Stage),
  hoverinfo = 'x+y+text'
  # hovertemplate = paste('<b>%{text}</b><br><b>X</b>: %{x}<br><b>Y</b>: %{y} <extra>tooltip</extra>')
) %>% 
  layout(
    legend = list(
      x = 0.1, y = 0.9, 
      title = list(text = "Legendary"), 
      bgcolor = "#E2E2E2"
    )
  )

## dropdown https://plotly.com/r/dropdowns
## buttons https://plotly.com/r/custom-buttons
plot_ly(
  data = df, 
  x = ~Type.1, 
  y = ~Attack,
  type = "box"
) %>% layout(
  title = "Attack distribution",
  xaxis = list(title = "TYPE"),
  yaxis = list(range = c(0, 140)),
  updatemenus = list(
    list(
      x = 1, y = 1,
      buttons = list(
        list(method = "restyle",
             args = list("type", "box"),
             label = "Boxplot"),
        list(method = "restyle",
             args = list("type", "violin"),
             label = "Violinplot")
      ))
  ))


# - #
pokemons_with_3stages <- na.omit(sapply(which(df$Stage == 1), function(i) ifelse(sum(df$Stage[i:(i+2)]) == 6, i, NA)))
pokemons_with_3stages_all <- c(pokemons_with_3stages, pokemons_with_3stages+1, pokemons_with_3stages+2)
pokemons_with_3stages_names <- paste0(df$Name[pokemons_with_3stages], "_", df$Name[pokemons_with_3stages+1], "_", df$Name[pokemons_with_3stages+2])
pokemons_with_3stages_all_names <- rep(pokemons_with_3stages_names, 3)

df_evolution <- df[pokemons_with_3stages_all,] %>%
  mutate(evolution = pokemons_with_3stages_all_names)
# - #


## sliders https://plotly.com/r/sliders
## range sliders https://plotly.com/r/range-slider
steps <- list(
  list(args = list(list("marker.color" = list("red"), 
                        "marker.symbol" = list("square"))),
       label = "Red Squares",
       method = "restyle",
       value = "1"
  ),
  list(args = list(list("marker.color" = list("black"), 
                        "marker.symbol" = list("diamond"))),
       label = "Black Diamonds",
       method = "restyle",
       value = "2"
  ),
  list(args = list(list("marker.color" = list("grey"), 
                        "marker.symbol" = list("circle"))), 
       label = "Grey Circles", 
       method = "restyle", 
       value = "3"
  )
)

plot_ly(
  data = df_evolution,
  x = ~Stage,
  y = ~Attack,
  color = ~evolution,
  colors = "Set1",
  type = "scatter",
  mode = "lines+markers",
  marker = list(size = 10)
) %>%
  layout(
    xaxis = list(tickvals = c(1, 2, 3)),
    sliders = list(
      list(
        active = 0,
        currentvalue = list(prefix = "Palette: "),
        pad = list(t = 60),
        steps = steps
      )
    )
  ) 


### Zadanie 1
## 1. Zwizualizować rozkłady 2-3 atrybutów dla wybranych pokemonów
##    https://plotly.com/r/statistical-charts
## 2. Dodać przycisk(i) zmieniające typ wykresu
##    https://plotly.com/r/custom-buttons



## animation https://plotly.com/r/animations
plot_ly(x = ~rnorm(50), type = "histogram")

X <- data.frame()
for (m in seq(-5, 5, 1)) {
  new_X <- data.frame(
    value = rnorm(100, m), 
    mean = m
  )
  X <- rbind(X, new_X)
}

fig <- plot_ly(data = X, x = ~value, frame = ~mean, type = "histogram")
fig

fig %>% 
  animation_opts(10) %>%
  animation_button(x = 0.05, y = 0.05) %>%
  animation_slider(currentvalue = list(prefix = "MEAN: ", font = list(color="red")))


### Zadanie 2
## Zanimować wykres z Zadania 1 po zmiennej Stage (lub Type/nowej stworzonej)




## https://plotly-r.com