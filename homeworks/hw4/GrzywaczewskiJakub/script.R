library(sf)
library(plotly)
library(mapview)
library(dplyr)
library(stringr)
library(rjson)

philly_map <- read_sf("./data/Philly/PhillyTotalPopHHinc.shp", quiet = TRUE)
class(philly_map)

plot_ly(philly_map)

sales_data <- read.csv("./data/Properties_philly_Kraggle_v2.csv")
# Filter nas in address column
sales_data <- sales_data %>%
  filter(!is.na(Address)) %>%
  filter(Address != "")

# Fetch geoid codes
addresses <- sales_data %>%  
  select(Address) %>%
  mutate(
    street = Address,
    city = "Philadelphia",
    state = "PA"
  )

addresses <- append_geoid(addresses, 'block group')

geoid_dict <- addresses %>% 
  mutate(Geoid = as.numeric(geoid) %/% 10) %>% 
  select(Address, Geoid) %>%
  filter(!is.na(Geoid)) %>%
  unique()

# Save for late use
write.csv(geoid_dict, "./data/Geoid_dict_philly.csv")

# Load from saved file
geoid_dict <- read.csv("./data/Geoid_dict_philly.csv")

# Pick interesting data and append geoids
convert_price_to_int <- function(price) {
  price <- str_remove_all(price, "\\$")
  price <- str_remove_all(price, ",")
  as.numeric(price)
}

philly_mean_price_map <- sales_data %>%
  select(Address, Sale.Price.bid.price) %>% 
  mutate(
    Price = convert_price_to_int(Sale.Price.bid.price),
    .keep = "unused"
  ) %>% 
  left_join(geoid_dict, by = "Address") %>%
  group_by(Geoid) %>%
  summarise(
    Mean_price = round(mean(Price)),
    Display_price = paste('$', formatC(Mean_price, big.mark=',', format = 'fg'))
  )

mean_price_data_map <- philly_map %>% 
  left_join(philly_mean_price_map, by = c("GEOID10" = "Geoid")) %>%
  mutate(Display_price = tidyr::replace_na(Display_price, "NA"))
  
mapview(mean_price_data_map, 
        zcol = "Mean_price",
        label= "Display_price",
        layer.name = "Mean price in district"
)

# plot_ly(mean_price_data_map, z = ~Mean_price)
# Why do you have to be like that plot_ly // Written at 2 PM

most_common <- function(x) {
  names(sort(table(x), decreasing = TRUE))[1]
}

philly_most_common_prop <- sales_data %>%
  select(Address, PropType, Sale.Price.bid.price) %>% 
  mutate(
    Price = convert_price_to_int(Sale.Price.bid.price),
    .keep = "unused"
  ) %>% 
  left_join(geoid_dict, by = "Address") %>%
  group_by(Geoid) %>%
  summarise(
    Prop = most_common(PropType),
    Mean_price = mean(Price),
    Display_price = paste('$', formatC(Mean_price, big.mark=',', format = 'fg'))
  )

prop_map <- philly_map %>% 
  left_join(philly_most_common_prop, by = c("GEOID10" = "Geoid")) %>%
  mutate(Prop = tidyr::replace_na(Prop, "NA"))
  
# Why????????
plot_ly(prop_map, color = ~Prop, text = ~Display_price, hoverinfo = "text" ) %>%
  layout(
    title = "Most common properties by sectors with mean price",
    legend = list(title=list(text='Properity Type'))
  )