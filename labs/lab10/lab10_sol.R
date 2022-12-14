###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 10           ###
###########################################

library(ggplot2)

#### leaflet ####

# https://rstudio.github.io/leaflet/
# install.packages("leaflet")

library(leaflet)

leaflet() %>%
  addTiles() %>% 
  addMarkers(lng = 21.007135613409062, lat = 52.22217811913538, popup = "Wydział MiNI")

# data.frame
df <- read.csv("https://raw.githubusercontent.com/MI2-Education/2023Z-DataVisualizationTechniques/main/homeworks/hw1/house_data.csv")
sam <- sample(1:nrow(df), 0.01 * nrow(df))
leaflet(df[sam,]) %>% 
  addTiles() %>% 
  addMarkers(lng = ~long, lat = ~lat)

# maps()
library(maps)

mapStates = map("state", fill = TRUE, plot = FALSE)

leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL))

## dodanie argumentu -> stroke = FALSE)

# points 
m = leaflet() %>% addTiles()
df = data.frame(
  lat = rnorm(100),
  lng = rnorm(100),
  size = runif(100, 5, 20),
  color = sample(colors(), 100)
)
m = leaflet(df) %>% addTiles()

m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)

m %>% addCircleMarkers(radius = runif(100, 4, 10), color = c('red'))


#### ggalluvial ####

# https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html
# install.packages("ggalluvial")

library(ggalluvial)

ggplot(as.data.frame(UCBAdmissions),
       aes(y = Freq, axis1 = Gender, axis2 = Dept)) +
  geom_alluvium(aes(fill = Admit), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)))


#### ggdist ####

# https://mjskay.github.io/ggdist/articles/dotsinterval.html
# install.packages("ggdist")

library(ggdist)

abc_df = data.frame(
  value = rnorm(300, mean = c(1,2,3), sd = c(1,2,2)),
  abc = c("a", "b", "c")
)

abc_df %>%
  ggplot(aes(x = abc, y = value)) +
  stat_dots(side = "both") +
  ggtitle('stat_dots(side = "both")')

data.frame(
  abc = c("a", "b", "b", "c"),
  value = rnorm(200, c(1, 8, 8, 3), c(1, 1.5, 1.5, 1))
) %>%
  ggplot(aes(y = abc, x = value, fill = abc)) +
  stat_slab(aes(thickness = stat(pdf*n)), scale = 0.7) +
  stat_dotsinterval(side = "bottom", scale = 0.7, slab_size = NA) +
  scale_fill_brewer(palette = "Set2") +
  ggtitle(paste0(
    'stat_slab(aes(thickness = stat(pdf*n)), scale = 0.7) +\n',
    'stat_dotsinterval(side = "bottom", scale = 0.7, slab_size = NA)'
  ),
  'aes(fill = abc)'
  )


#### ggbump ####

# https://github.com/davidsjoberg/ggbump
# install.packages("ggbump")

library("ggbump")

df <- data.frame(country = rep(c("Indie", "Szwecja", "Niemcy"), each = 3),
                 year = rep(c(2011, 2012, 2013), 3),
                 value = round(runif(9, 300, 800), 0)
                 )

df %>% 
  group_by(year) %>% 
  mutate(rank = rank(value, ties.method = "random")) %>% 
  ungroup() %>% 
  ggplot(aes(year, rank, color = country)) +
  geom_bump()


#### ggbeeswarm ####

# https://github.com/eclarke/ggbeeswarm
#install.packages("ggbeeswarm")

library(ggbeeswarm)

# geom_jitter()
ggplot(iris,aes(Species, Sepal.Length)) + geom_jitter()

# ggbeeswarm

ggplot(iris,aes(Species, Sepal.Length)) + geom_quasirandom()


#### ggridges ####

# https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
# install.packages("ggridges")

library(ggridges)

data <- data.frame(x = 1:5, y = rep(1, 5), height = c(0, 1, 3, 4, 2))

ggplot(data, aes(x, y, height = height)) + geom_ridgeline()

ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges()

ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges(scale = 1)

ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges(scale = 5)

#### visNetwork ####

# https://datastorm-open.github.io/visNetwork/
# install.packages("visNetwork")

library(visNetwork)

nodes <- data.frame(id = 1:8, 
                    label = c("Białystok", "Warszawa",
                              "Radom", "Sosnowiec",
                              "Kraków", "Wrocław", 
                              "Gdańsk", "Lądek Zdrój"))

edges <- data.frame(from = c(2, 2, 2, 2, 3, 3, 3, 4, 4, 5, 2), 
                    to = c(6, 7, 3, 4, 4, 5, 6, 5, 6, 6, 8), 
                    color = "red",
                    label = "droga",
                    title = "Miasta")

net <- visNetwork(nodes, edges, height = 600, width = 1000) %>%
  visEdges(arrows = "from")  %>% 
  visLayout(randomSeed = 123) 

net


# Zadanie 
# Stwórz własny graf przedstawiający stacje metra w Warszawie (obie linie) 
# i zapisz go do pliku html (funkcja visSave).

nodes <- data.frame(id = 1:12, 
                    label = c("Pole Mokotowskie", "Politechnika",
                              "Centrum", "Świętokrzyska",
                              "Ratusz Arsenał", "Dworzec Gdański", 
                              "Rondo ONZ", "Rondo Daszyńskiego", 
                              "Płocka", "Młynów",
                              "Nowy Świat-Uniwersytet", "Centrum Nauki Kopernik"))

edges <- data.frame(from = c(1, 2, 3, 4, 5, 4, 7, 8, 9, 4, 11), 
                    to = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

net <- visNetwork(nodes, edges, height = 600, width = 1000) %>%
  visEdges(arrows = "from")  %>% 
  visLayout(randomSeed = 123) 

net
