###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 11           ###
###########################################

## 0) dane 
library(dplyr)
library(ggplot2)
df <- readr::read_csv("https://raw.githubusercontent.com/mini-pw/2022Z-DataVisualizationTechniques/main/labs/lab11/covid.csv") %>%
  select(location, date, 
         total_cases_per_million,
         total_deaths_per_million,
         new_deaths_per_million,
         people_fully_vaccinated_per_hundred,
         human_development_index,
         median_age)

dim(df)
colnames(df)

## 1) gganimate
# https://gganimate.com/articles/gganimate.html
library(gganimate)
library(Rcpp)
library(gifski)
library(av)


?transition_reveal
p_line <- ggplot(df, aes(x = total_cases_per_million, 
                         y = total_deaths_per_million)) +
  geom_line(aes(group = location))

p_line

anim_line <- p_line + 
  transition_reveal(date)
anim_line


?transition_states
p_point <- ggplot(df, aes(x = total_cases_per_million, 
                          y = total_deaths_per_million)) +
  geom_point(aes(group = location))

p_point

anim_point <- p_point +
  labs(title = "{closest_state}") + 
  transition_states(date,
                    transition_length = 2,
                    state_length = 1)
animate(anim_point) # default nframes = 100
animate(anim_point, nframes = 2*length(unique(df$date)))


anim_point2 <- p_point +
  geom_text(aes(label = location), nudge_y = 100) +
  # ggrepel::geom_text_repel(aes(label = location)) +
  labs(title = "{previous_state} - {next_state}") + 
  transition_states(date)
anim_point2


p_hdi <- ggplot(df, aes(x = total_cases_per_million, 
                        y = total_deaths_per_million)) +
  geom_point(aes(color = human_development_index)) +
  scale_colour_gradient2(midpoint = mean(df$human_development_index, na.rm = T)) +
  theme_dark()
p_hdi


df_small <- df %>% 
  filter(location %in% c("Greece", "Ukraine", "Denmark", "Germany", "Austria",
                         "Poland")) %>%
  mutate(location = forcats::fct_reorder(location, total_deaths_per_million))

p_age <- ggplot(df_small, 
                aes(x = location, 
                    y = total_deaths_per_million)) +
  geom_col(aes(fill = people_fully_vaccinated_per_hundred)) +
  scale_fill_gradient2(midpoint = 50) +
  theme_dark() +
  coord_flip()
p_age

p_age + 
  labs(title = "{current_frame}") +
  transition_manual(date)


# cumulative
# w tym wypadku nie usuwa słupków gdy maleje
ggplot(df_small, 
       aes(x = location, 
           y = total_deaths_per_million)) +
  geom_col(aes(fill = people_fully_vaccinated_per_hundred)) +
  scale_fill_gradient2(midpoint = 50) +
  theme_dark() +
  coord_flip() +
  labs(title = "{current_frame}") +
  transition_manual(date, cumulative = T)



## 2) rbokeh
# https://hafen.github.io/rbokeh/articles/rbokeh.html
library(rbokeh)

figure() %>% 
  ly_points(birth.rate, death.rate, data = SmarterPoland::countries, 
            color = continent)


## 3) ggiraph
# https://davidgohel.github.io/ggiraph/articles/offcran/using_ggiraph.html
library(ggiraph)

df_ggiraph <- SmarterPoland::countries %>%
  mutate(onclick = sprintf("window.open(\"%s%s\")",
                           "http://en.wikipedia.org/wiki/", 
                           as.character(gsub("'", "&#39;", country))))

my_gg <- ggplot(df_ggiraph, 
                aes(x = birth.rate, y = death.rate, color = continent)) +
  geom_point_interactive(aes(tooltip = country, onclick = onclick))

girafe(code = print(my_gg))


## 4) vegalite
# https://github.com/hrbrmstr/vegalite
devtools::install_github("hrbrmstr/vegalite")
library(vegalite)

dat <- jsonlite::fromJSON('[
    {"a": "A","b": 28}, {"a": "B","b": 55}, {"a": "C","b": 43},
    {"a": "D","b": 91}, {"a": "E","b": 81}, {"a": "F","b": 53},
    {"a": "G","b": 19}, {"a": "H","b": 87}, {"a": "I","b": 52}
  ]')

vegalite() %>%
  add_data(dat) %>%
  encode_x("a", "ordinal") %>%
  encode_y("b", "quantitative") %>%
  mark_bar() -> vl

vl

## 5) googleVis
# https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html
library(googleVis)

df = data.frame(
  country = c("US", "GB", "BR"),
  val1 = c(10, 13, 14),
  val2 = c(23, 12, 32)
)

bar <- gvisBarChart(df)
plot(bar)
