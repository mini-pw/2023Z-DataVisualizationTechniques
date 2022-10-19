library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

df$long


# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej medianyi
# położonych na wschód od południka 122W?
df %>%
  filter(bathrooms > median(bathrooms) & long > -122) %>%
  summarise(mean(price))

# Odp: 625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?
df %>%
  count(yr_built) %>%
  filter(n == max(n))
  
# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w 
#porównaniu z tymi położonymi nie nad wodą?
x = df %>%
  group_by(waterfront) %>%
  summarise(medianPrice = median(price))
x
x$medianPrice[2]/x$medianPrice[1]

# Odp: 211%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych 
#nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
df %>% 
  filter(floors == 1) %>%
  group_by(yr_built) %>%
  filter(price == min(price)) %>%
  ungroup() %>%
  summarise(mean(sqft_living))
  

# Odp: 1030

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości 
#wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? 
#Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?
df %>%
  filter(bathrooms == 1 | bathrooms == 2) %>%
  group_by(bathrooms) %>%
  summarise(Q1 = quantile(grade, 1/4), Q3 = quantile(grade, 3/4))
  

# Odp: Tak. Jedna - Q1:6; Q2:7
#           Dwie - Q1:7; Q2:8

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy 
#a jaki tych na południu? (Północ i południe definiujemy jako położenie 
#odpowiednio powyżej i poniżej punktu znajdującego się w połowie między 
#najmniejszą i największą szerokością geograficzną w zbiorze danych)
orientationPoint = (max(df$lat) + min(df$lat))/2
df %>% 
  mutate(NS = ifelse(lat > orientationPoint, "North", "South")) %>%
  group_by(NS) %>%
  summarise(iqr = IQR(price))

# Odp: powyżej: 321000, pniżej: 122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach
#niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację
#nie przekracza 1800 sqft?
df %>%
  filter(waterfront == 0) %>%
  select(bathrooms, floors, sqft_living) %>%
  mutate(sqftl_per_floor = sqft_living /floors) %>%
  filter(sqftl_per_floor <= 1800) %>%
  count(bathrooms) %>%
  filter(n == max(n) | n == min(n))

# Odp: najczęściej 2,5; najrzadziej 4,75

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. 
#Dla każdego z nich podaj odchylenie standardowe powierzchni działki 
#oraz najpopularniejszą liczbę łazienek
x <- df %>%
  group_by(zipcode) %>%
  count(zipcode) %>%
  filter(n > 550) %>% 
  select(zipcode) %>%
  merge(df)

x %>%
  group_by(zipcode) %>%
  summarise(diviation = sd(sqft_lot))

x %>%
  group_by(zipcode, bathrooms) %>%
  count(bathrooms) %>%
  group_by(zipcode) %>%
  summarise(bathrooms[n == max(n)])
  
# Odp:
#1   98038                2.5     63111
#   98052                 2.5     10276
#   98103                 1       1832
#   98115                 1       2675
#   98117                 1       2319

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia 
#mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] 
#oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.
df %>% 
  filter(waterfront == 0) %>%
  mutate(category = ifelse(sqft_living <= 2000, "Low", 
                    ifelse(sqft_living <= 4000, "Medium", "High"))) %>%
  select(price, category) %>%
  group_by(category) %>%
  summarise(mean = mean(price), median = median(price))
  
# Odp:  (0, 2000] - średnia: 385084; mediana:359000 
#       (2000,4000] - średnia: 645419; mediana:595000
#       (4000, +Inf) -  średnia: 1448119; mediana:1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? 
#(bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
df %>%
  transmute(price_per_m2 = price / (sqft_living*0.0929)) %>%
  min()

# Odp: 942.82

