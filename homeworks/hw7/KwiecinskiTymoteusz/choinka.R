library(leaflet)
library(sf)
library(dplyr)

powiaty <- geojsonio::geojson_read("powiaty-min.geojson.geojson", what="sp")

class(powiaty)
names(powiaty)

powiaty$col = rep(10, 380)

greens <- c(45, 26, 8, 285, 5, 371, 95, 177,231,135,366,242,235,265,174,160,287,
            21, 159, 212, 167, 153, 137, 295, 15, 161, 282, 97, 85, 193, 305, 15,
            314, 315, 347, 142, 224, 175, 111, 184, 31, 223, 19, 46, 43, 165, 118,
            219, 280, 165, 118, 92, 33, 264, 348, 294, 298, 62, 110, 346, 106, 
            140, 83, 6, 168, 339, 253, 166, 322, 131, 293, 297, 296, 191, 68, 9,
            288, 58, 380, 320, 162, 312, 119, 143, 82, 139, 232, 121, 3, 57, 44,
            196, 199, 198, 319, 363, 29, 306, 244, 338, 214, 266, 333, 64, 18, 243,
            302, 20, 4, 215, 115, 47, 370)
browns <- c(146, 147, 271, 316, 241, 148, 344, 149, 359, 144, 148, 87, 38, 42, 
            90, 41, 192, 40, 358, 317, 250)

yellows <- c(308, 377, 141, 237, 88)


for (g in greens) {
  powiaty$col[g] = 1
}

for (b in browns) {
  powiaty$col[b] = 4
}


for (y in yellows) {
  powiaty$col[y] = 5
}

for (i in powiaty$id){
  if(tolower(powiaty$nazwa[i]) != powiaty$nazwa[i]) # czyli czy to jest miasto na prawach powiatu
    {if(powiaty$col[i] == 1){
      powiaty$col[i] = 3
    }
    if(powiaty$col[i] == 10){
      powiaty$col[i] = 2
    }
  }
}

powiaty$color=case_when(
  powiaty$col == 1 ~ "green",
  powiaty$col == 2 ~ "white",
  powiaty$col == 3 ~ "blue",
  powiaty$col == 4 ~ "brown",
  powiaty$col == 5 ~ "yellow",
  TRUE ~ "black"
)

labels <- sprintf(
  "<strong>%g</strong><br/>%s",
  powiaty$id, powiaty$nazwa
) %>% lapply(htmltools::HTML)


m <- leaflet(powiaty) %>% 
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>%  
  addPolygons(
  fillColor = ~color,
  fillOpacity = 0.8,
  weight = 0.4,
  color = "white",
  opacity = 1,
  highlightOptions = highlightOptions(color='white',weight=1,
                                      bringToFront = TRUE),
  label = labels)
m

