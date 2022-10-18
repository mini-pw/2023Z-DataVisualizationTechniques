library(dplyr)

df <- read.csv("house_data.csv")
colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?
df %>% 
  filter(bathrooms > median(bathrooms),long > -122) %>%
  summarise(srednia_cena = mean(price))


# Odp: 625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?
df %>% 
  group_by(yr_built) %>% 
  summarize(n = n()) %>% 
  arrange(-n) %>% 
  head(1)

# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
df %>% 
  filter(waterfront == 1) %>% 
  summarize(mediana_woda = median(price)) -> mediana_woda 
df %>% 
  filter(waterfront == 0) %>% 
  summarize(mediana_nie_woda = median(price)) -> mediana_nie_woda 
res = round(mediana_woda / mediana_nie_woda,2)*100 - 100
# Odp: ~211%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
df %>%
  group_by(yr_built) %>%
  filter(floors == 1) %>% 
  filter(price == min(price)) %>%
  ungroup() %>% 
  summarize(srednia = mean(df4$sqft_living)) %>% 
  as.data.frame()


# Odp: 1030.42

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?
quant_1 <- quantile(df[df$bathrooms == 1,]$grade)
quant_2 <- quantile(df[df$bathrooms == 2,]$grade)


# Odp: Tak różni się. Dla nieruchomości z jedną łazienką 1. kwartyl jakości wykończenia wynosi 6
# a 3. wynosi 7. Dla nieruchomości z dwiema łazienkami 1. kwartyl wynosi 7 a 3. 8 

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)
min_lat <- min(df$lat)
max_lat <- max(df$lat)
mid_lat <- (max_lat + min_lat) / 2

flats_north <- df %>%
  filter(lat > mid_lat)

flats_south <- df %>%
  filter(lat < mid_lat)

quant_north = quantile(flats_north$price)
quant_north_dif = quant_north[4] - quant_north[2]

quant_south = quantile(flats_south$price)
quant_south_dif = quant_south[4] - quant_south[2]
  

# Odp: Odstęp międzykwartylowy ceny dla mieszkań położonych na północy wynosi 321000 a dla
# mieszkań połóżonych na południu wynosi 122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
df7 <- df %>% 
  filter(waterfront == 0) %>% 
  filter(sqft_living / floors <= 1800) %>% 
  group_by(bathrooms) %>% 
  summarize(n = n()) %>% 
  arrange(n)

head(df7,1)
tail(df7,1)

# Odp: Najczęściej występuje 2.5 łazienek a najrzadziej 4.75

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
df_zipcodes <- df %>% 
  group_by(zipcode) %>% 
  summarize(n = n()) %>% 
  filter(n > 550) 

zipcodes <- pull(df_zipcodes, zipcode)

df %>% 
  filter(zipcode %in% zipcodes) %>% 
  group_by(zipcode) %>% 
  summarize(odchylenie_standardowe = sd(sqft_lot))

df %>%
  filter(zipcode %in% zipcodes) %>% 
  group_by(zipcode, bathrooms) %>%
  summarize(n = n()) %>% 
  slice(which.max(n))

# Odp: 
# zipcode odchylenie_standardowe
#    <int>                  <dbl>
#1   98038                 63111.
#2   98052                 10276.
#3   98103                  1832.
#4   98115                  2675.
#5   98117                  2319.

#   zipcode  najpopularniejsza l. łazienek     
#    <int>      <dbl> 
#1   98038       2.5   
#2   98052       2.5   
#3   98103       1     
#4   98115       1     
#5   98117       1     

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.
df %>% 
  filter(waterfront == 0) %>% 
  mutate(sqft_band = case_when(sqft_living <= 2000 & sqft_living > 0 ~ "(0, 2000]",
                                sqft_living <= 4000 & sqft_living > 2000 ~ "(2000, 4000]",
                                sqft_living > 4000 ~ "(4000, +Inf)")) %>% 
  group_by(sqft_band) %>% 
  summarise(mediana = median(price), srednia = mean(price))

# Odp:
#  sqft_band    mediana  srednia
#   <chr>          <dbl>    <dbl>
#1 (0, 2000]     359000  385084.
#2 (2000, 4000]  595000  645419.
#3 (4000, +Inf) 1262750 1448119.

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
df %>%
  transmute(cena_za_m = price / (sqft_living * 0.09290304)) %>% 
  arrange(cena_za_m) %>% 
  head(1)


# Odp: 942.7919
