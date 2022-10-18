library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany 
# i położonych na wschód od południka 122W?
median(df$bathrooms)

df %>% 
  filter( df$bathrooms > median(df$bathrooms) & df$long > -122) %>% 
  summarise(mean = mean(price))
# Odp: 625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>% 
  group_by(yr_built) %>% 
  summarise(n = n()) %>%
  top_n(1)

# Odp:
# yr_built     n
# 2014        559

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
df %>% 
  group_by(waterfront) %>% 
  summarise(mediana = median(price)) -> temp
  
  (temp[2,2] - temp[1,2])/temp[1,2]*100

# Odp: 211.1111%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter)
# wybudowanych w każdym roku?
# TODO
df %>% 
  filter(df$floors == 1) %>% 
  group_by(yr_built) %>% 
  filter(price == min(price)) %>% 
  select(id,sqft_living,price) %>%
  arrange(yr_built) %>% 
  ungroup() %>% 
  summarise(avg_sqft = mean(sqft_living))

# Odp: Średnia powierzechnia takich nieruchomości wynosi 1030 sqft.

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami
# z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

df %>% 
  filter(df$bathrooms %in% c(1,2)) %>% 
  group_by(bathrooms) %>% 
  summarise(first = quantile(grade, 0.25), third = quantile(grade,0.75))

# Odp: Jest. Wynosi 1
# bathrooms first third
# 1           6     7
# 2           7     8

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu?
# (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się
# w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)
# TODO

# df %>%
#   summarise(min = min(lat), max = max(lat)) %>% 
#   mutate(srednia= (min+max)/2)

centrum = (min(df$lat) + max(df$lat))/2
  
df %>%
  select(id,price,lat) %>% 
  mutate(czy_polnoc = ifelse(lat > centrum, TRUE, FALSE)) %>%
  group_by(czy_polnoc) %>% 
  summarise(odstep = IQR(price))
  
  
?IQR
# Odp: Odstęp międzykwartylowy wynosi: 
# dla południa: 122500
# dla północy: 321000

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą,
# których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?

df %>% 
  filter(df$waterfront == 0, df$sqft_living/(df$floors) <= 1800) %>% 
  group_by(bathrooms) %>% 
  summarise(n = n()) %>%
  arrange(n) -> temp

temp[c(1,dim(temp)[1]),]  

# Odp: Najczęściej 2.5 (4308), najrzadziej 4.75 (1).

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości.
# Dla każdego z nich podaj odchylenie standardowe powierzchni działki
# oraz najpopularniejszą liczbę łazienek

df %>% 
  group_by(zipcode) %>% 
  summarise(n = n()) %>% 
  filter(n > 550) -> zipcodes
zipcodes
  
df %>% 
  select(id,sqft_lot,zipcode) %>% 
  filter(df$zipcode %in% zipcodes$zipcode) %>% 
  group_by(zipcode) %>% 
  summarise(odchylenie = sd(sqft_lot))

df %>% 
  select(id,bathrooms,zipcode) %>% 
  filter(df$zipcode %in% zipcodes$zipcode) %>% 
  group_by(zipcode, bathrooms) %>% 
  summarise(n = n()) %>% 
  top_n(1)


# Odp:
# Odchylenie standardowe:
# zipcode   odchylenie
# 98038     63111.
# 98052     10276.
# 98103      1832.
# 98115      2675.
# 98117      2319.

# Najpopularniejsza liczba łazienek
# zipcode   bathrooms     n
# 98038       2.5       334
# 98052       2.5       216
# 98103       1         167
# 98115       1         162
# 98117       1         178


# 9. Porównaj średnią oraz medianę ceny nieruchomości,
# których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000]
# oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

df %>% 
  select(id,price,sqft_living,waterfront) %>%
  filter(waterfront == 0) %>% 
  mutate(przedzial = ifelse(sqft_living <= 2000, "(0, 2000]",ifelse(sqft_living <= 4000, "(2000,4000]","(4000, +Inf)"))) %>% 
  group_by(przedzial) %>% 
  summarise(srednia = mean(price), mediana = median(price))

# Odp:
# przedzial     srednia mediana
# (0, 2000]     385084.  359000
# (2000,4000]   645419.  595000
# (4000, +Inf) 1448119. 1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości?
# (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df %>% 
  select(id,price,sqft_living) %>% 
  mutate(sq_meters = sqft_living*0.09290304) %>% 
  mutate(price_per_sqft = price/sq_meters) %>% 
  summarise(min = min(price_per_sqft))

# Odp: 942.7919

