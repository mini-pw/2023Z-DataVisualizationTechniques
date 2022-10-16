library(dplyr)

house_data <- read.csv("./house_data.csv")
colnames(house_data)
dim(house_data)
apply(house_data, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?
house_data %>% 
  filter(bathrooms > median(bathrooms)) %>% 
  filter(long > -122) %>% 
  summarise(mean_price = mean(price))

# Odp: 1   625499.4


# 2. W którym roku zbudowano najwięcej nieruchomości?
house_data %>% 
  group_by(yr_built) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  slice(1) %>% 
  select(yr_built)

# Odp: 1     2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
seaside <- house_data %>% 
  filter(waterfront == 1) %>% 
  summarise(median_seaside = median(price))

not_seaside <- house_data %>% 
  filter(waterfront != 1) %>% 
  summarise(median_not_seaside = median(price))

(seaside-not_seaside)/not_seaside*100

# Odp: 1       1       211.1111

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
house_data %>% 
  filter(floors == 1) %>% 
  select(yr_built, sqft_living) %>% 
  arrange(yr_built) %>% 
  group_by(yr_built) %>%
  summarise(average_sqft = mean(sqft_living)) %>% 
  arrange(average_sqft)

# Odp: 
# yr_built average_sqft
# <int>        <dbl>
# 1     1935        1025 
# 2     1905        1058.
# 3     1934        1099.
# 4     1907        1115.
# 5     1918        1135.
# 6     1902        1150 
# 7     1943        1153.
# 8     1903        1162.
# 9     1932        1178.
# 10    1909        1179.

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?
one_bathroom <- house_data %>% 
  filter(bathrooms == 1)

summary(one_bathroom$grade)

two_bathroom <- house_data %>% 
  filter(bathrooms == 2)

summary(two_bathroom$grade)

# Odp: Dla nieruchomości z jedną łazienką w 1 kwartylu jakość 
# wykończenia jest równa 6, a w trzecim kwartylu 7.
# W nieruchomościach z dwoma łazienkami jakość
# wykończenia w pierwszym kwartylu jest równa 7, a w trzecim kwartylu 8.

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)
min_lat = min(house_data$lat)
max_lat = max(house_data$lat)
midddle_lat = (max_lat + min_lat) / 2

north_flats <- house_data %>% 
  filter(lat > midddle_lat)

south_flats <- house_data %>% 
  filter(lat < midddle_lat)

summary(north_flats$price)[5] - summary(north_flats$price)[2]
summary(south_flats$price)[5] - summary(south_flats$price)[2]

# Odp: Dla mieszkań na północy odstęp wynosi 321000, a dla mieszkań na południu 122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
house_data %>% 
  filter(waterfront == 0) %>% 
  filter(sqft_living/floors <= 1800) %>% 
  group_by(bathrooms) %>% 
  summarise(n=n()) %>% 
  slice(which.max(n))

house_data %>% 
  filter(waterfront == 0) %>% 
  filter(sqft_living/floors <= 1800) %>% 
  group_by(bathrooms) %>% 
  summarise(n=n()) %>% 
  slice(which.min(n))

# Odp: Najczęściej 2.5 łazienki, najrzadziej 4.75 łazienki

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
zipcodes <- house_data %>% 
  select(zipcode, sqft_lot, bathrooms) %>% 
  group_by(zipcode) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>% 
  filter(n>550)

zipcodes_vector <- pull(zipcodes, zipcode)

house_data %>% 
  filter(zipcode %in% zipcodes_vector) %>% 
  group_by(zipcode) %>% 
  summarise(standard_deviation = sd(sqft_lot), n=n())

house_data %>% 
  filter(zipcode %in% zipcodes_vector) %>% 
  group_by(zipcode, bathrooms) %>% 
  summarise(n=n()) %>% 
  group_by(zipcode) %>% 
  slice(which.max(n))

# Odp: zipcode standard_deviation
# <int>              <dbl>
# 1   98038             63111.
# 2   98052             10276.
# 3   98103              1832.
# 4   98115              2675.
# 5   98117              2319.

# zipcode bathrooms     n
# <int>     <dbl> <int>
# 1   98038       2.5   334
# 2   98052       2.5   216
# 3   98103       1     167
# 4   98115       1     162
# 5   98117       1     178

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.
house_data %>% 
  select(price, sqft_living, waterfront) %>% 
  filter(waterfront == 0) %>% 
  mutate(sqft_group = case_when(sqft_living<=2000 & sqft_living>0 ~ "(0, 2000]",
                                sqft_living<=4000 & sqft_living>2000 ~ "(2000, 4000]",
                                sqft_living>4000 ~ "(4000, +Inf)")) %>% 
  group_by(sqft_group) %>% 
  summarise(median = median(price), mean = mean(price))
  

# Odp:   sqft_group    median     mean
#       <chr>          <dbl>    <dbl>
#       1 (0, 2000]     359000  385084.
#       2 (2000, 4000]  595000  645419.
#       3 (4000, +Inf) 1262750 1448119.

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
house_data %>% 
  select(price, sqft_living) %>% 
  mutate(price_per_meter = price/(sqft_living/10.764)) %>% 
  arrange(price_per_meter) %>% 
  head(1)

# Odp: price_per_meter
#      942.7998

