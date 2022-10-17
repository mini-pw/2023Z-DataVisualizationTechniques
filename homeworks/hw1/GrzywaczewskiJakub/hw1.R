library(dplyr)

df <- read.csv("../house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w zadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczba łazienek powyzej mediany i połozonych na wschod od południka 122W?
hist(df$long)
df %>% 
  filter(bathrooms > median(bathrooms) & long > -122) %>%
  summarise(mean_med = mean(price))

# Odp: 625499.4 USD

# 2. W ktorym roku zbudowano najwiecej nieruchomości?
df %>%
  group_by(yr_built) %>%
  summarise(n = n()) %>% 
  slice_max(n, n = 1)

# Odp: 2014

# 3. O ile procent wieksza jest mediana ceny budynkow połozonych nad woda w porownaniu z tymi połozonymi nie nad woda?
df %>%
  group_by(waterfront) %>%
  summarise(med = median(price, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = waterfront, values_from = med, names_prefix = "water") %>%  
  summarise(inc = 100 * (water1 - water0) / water0)

# Odp: O około 211%

# 4. Jaka jest średnia powierzchnia wnetrza mieszkania dla najtanszych nieruchomości posiadajacych 1 pietro (tylko parter) wybudowanych w kazdym roku?

df %>%
  filter(floors == 1) %>%
  select(price, yr_built, sqft_living) %>%
  group_by(yr_built) %>%
  top_n(-1, price) %>% 
  ungroup() %>% 
  summarise(srednia = mean(sqft_living))

# Odp: 1030 stop kwadratowych

# 5. Czy jest roznica w wartości pierwszego i trzeciego kwartyla jakości wykonczenia pomieszczen pomiedzy nieruchomościami z jedna i dwoma łazienkami? Jeśli tak, to jak rozni sie Q1, a jak Q3 dla tych typow nieruchomości?
df %>%
  filter(bathrooms == 1 | bathrooms == 2) %>% 
  select(grade) %>%
  summary()

# Odp: Tak, roznia sie o 1

# 6. Jaki jest odstep miedzykwartylowy ceny mieszkan połozonych na połnocy a jaki tych na południu? (Połnoc i południe definiujemy jako połozenie odpowiednio powyzej i ponizej punktu znajdujacego sie w połowie miedzy najmniejsza i najwieksza szerokościa geograficzna w zbiorze danych)

boxplot(df$lat)
midpoint <- (max(df$lat) + min(df$lat)) / 2
df %>%
  select(price, lat) %>%
  mutate(
    north = ifelse(lat > midpoint, price, NA),
    south = ifelse(lat < midpoint, price, NA),
    .keep = "none"
  ) %>%
  summarise(
    north_irq = IQR(north, na.rm = TRUE),
    south_irq = IQR(south, na.rm = TRUE)
  )
  
# Odp: Na połnocy ostep miedzykwartylowy wynosi 321000 a na południu 122500

# 7. Jaka liczba łazienek wystepuje najcześciej i najrzadziej w nieruchomościach niepołozonych nad woda, ktorych powierzchnia wewnetrzna na kondygnacje nie przekracza 1800 sqft?
bathroom_data <- df %>% 
  filter(waterfront == 0) %>%
  mutate(sqft_per_floor = sqft_living / floors) %>% 
  filter(sqft_per_floor <= 1800) %>% 
  group_by(bathrooms) %>% 
  summarise(n = n()) %>% 
  arrange(n)

top_n(bathroom_data, 1)
top_n(bathroom_data, -1)

# Odp: Najcześciej wsytepuje 2.5 lazienek a najrzadziej 4.75 lazienek

# 8. Znajdz kody pocztowe, w ktorych znajduje sie ponad 550 nieruchomości. Dla kazdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejsza liczbe łazienek
most_common <- function(x){
  res <- sort(table(x),decreasing=TRUE)[1]
  as.numeric(names(res))
}

df %>% 
  group_by(zipcode) %>% 
  summarise(
    count = n(),
    most_common = most_common(bathrooms),
    sd = sd(sqft_lot)
  ) %>%
  filter(count > 550)

# Odp:
# zipcode count most_common     sd
#   <int>   <int>     <dbl>  <dbl>
#   98038     590       2.5 63111.
#   98052     574       2.5 10276.
#   98103     602       1    1832.
#   98115     583       1    2675.
#   98117     553       1    2319.

# 9. Porownaj średnia oraz mediane ceny nieruchomości, ktorych powierzchnia mieszkalna znajduje sie w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujacych sie przy wodzie.
df %>%
  filter(waterfront == 0) %>%
  mutate(
    "0-2000" = ifelse(sqft_living > 0 & sqft_living <= 2000, price, NA),
    "2000-4000" = ifelse(sqft_living > 2000 & sqft_living <= 4000, price, NA),
    "4000-inf" = ifelse(sqft_living > 4000, price, NA),
    .keep = "used"
  ) %>%
  summary()

# Odp: 
#             0-2000     2000-4000    4000-inf
#   Średnia:  385084        645419    1448119
#   Mediana:  359000        595000    1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwage tylko powierzchnie wewnatrz mieszkania)
CONV_RATE = 0.09290304 # 1 sqft w metrach kwadratowych
df %>% 
  mutate(sqm_living = sqft_living * CONV_RATE) %>% 
  summarise(price_per_sqm = price / sqm_living) %>% 
  slice_min(price_per_sqm, n = 1)
  
# Odp: 942.79
