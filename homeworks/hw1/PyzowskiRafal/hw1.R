library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i
# położonych na wschód od południka 122W?

df %>% 
  select(price, bathrooms, long) %>% 
  filter(bathrooms > median(bathrooms) & long > -122) %>% 
  select(price) -> temp
mean(temp$price)

# Odp: 625499.4 

# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>% 
  group_by(yr_built) %>% 
  summarise(count = n()) %>% 
  top_n(1, count)
  
# Odp: Najwięcej nieruchomości zbudowano w 2014 roku

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w 
# porównaniu z tymi położonymi nie nad wodą?

df %>% 
  filter(waterfront == 1) %>% 
  select(price) -> wat_1
med_wat_1 <- median(wat_1$price)

df %>% 
  filter(waterfront == 0) %>% 
  select(price) -> wat_0
med_wat_0 <- median(wat_0$price)

med_wat_1 / med_wat_0 * 100

# Odp: mediana cen budynków nad wodą jest o 311.1111% większa niż tych nie nad wodą

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych 
# nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?

summary(df$price)
# zakładam, że najtańsze mieszkania, to 25% najtańszych mieszkań (pierwsze kwartyle)

df %>% 
  filter(floors == 1 & price <= 321950) %>% 
  group_by(yr_built) %>% 
  summarise(mean = mean(sqft_living))

# Odp: dla kilku lat:
#       yr_built  mean
#      <int> <dbl>
# 1     1900 1058.
# 2     1902 1085 
# 3     1903  764 
# 4     1904  958 
# 5     1905 1185 
# 6     1906  857.
# 7     1907  932 
# 8     1908  986.
# 9     1909 1080 
# 10     1910 1034.
# # ... with 102 more rows

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości 
# wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? 
# Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

df %>% 
  filter(bathrooms == 1) -> bath_1
df %>% 
  filter(bathrooms == 2) -> bath_2

summary(bath_1$grade)
summary(bath_2$grade)

# > summary(bath_1$grade)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.000   6.000   7.000   6.564   7.000   9.000 
# > summary(bath_2$grade)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.000   7.000   7.000   7.241   8.000  10.000 

# Odp: Dla nieruchomości z dwiema łazienkami Q1 i Q3 jest o 1 punkt wyższy 
# w porównaniu z nieruchomościami z jedną łazienką

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a 
# jaki tych na południu? (Północ i południe definiujemy jako położenie 
# odpowiednio powyżej i poniżej punktu znajdującego się w połowie między 
# najmniejszą i największą szerokością geograficzną w zbiorze danych)

cent <- (max(df$lat) + min(df$lat))/2

df %>% 
  filter(lat > cent) %>% 
  select(price) -> north
df %>% 
  filter(lat < cent) %>% 
  select(price) -> south

summary(north)
summary(south)
# > summary(north)
# price        
# Min.   :  78000  
# 1st Qu.: 390000  
# Median : 521000  
# Mean   : 609070  
# 3rd Qu.: 711000  
# Max.   :7700000  
#
# > summary(south)
# price        
# Min.   :  75000  
# 1st Qu.: 245000  
# Median : 295916  
# Mean   : 327767  
# 3rd Qu.: 367500  
# Max.   :2510000  
711000 - 390000  
367500 - 245000 

# Odp: na północy 321000, a na południu 122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach 
# niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie 
# przekracza 1800 sqft?

df %>% 
  filter(waterfront == 0 & (sqft_living / floors) <= 1800) %>% 
  group_by(bathrooms) %>% 
  summarise(count = n()) %>% 
  top_n(1, count)

# Odp: najczęściej wysyępuje 2,5 łazienki

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. 
# Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz 
# najpopularniejszą liczbę łazienek

df %>% 
  group_by(zipcode) %>% 
  summarise(n = n()) %>% 
  filter(n > 550) -> zipcodes
#   zipcode     n
#     <int> <int>
# 1   98038   590
# 2   98052   574
# 3   98103   602
# 4   98115   583
# 5   98117   553

for(code in zipcodes$zipcode) {
  df %>%
    filter(zipcode == code) -> temp
  cat("odchylenie startadowe powierzchni działki dla kodu: ", code, "\n")
  print(sd(temp$sqft_lot))
  
  temp %>% 
    group_by(bathrooms) %>% 
    summarise(count = n()) %>% 
    top_n(1, count) -> temp
  print('najpopularniejsza liczba łazieniek')
  print(temp$bathrooms)
}

# Odp: 
# odchylenie startadowe powierzchni działki dla kodu:  98038 
# [1] 63111.11
# [1] "najpopularniejsza liczba łazieniek"
# [1] 2.5
# odchylenie startadowe powierzchni działki dla kodu:  98052 
# [1] 10276.19
# [1] "najpopularniejsza liczba łazieniek"
# [1] 2.5
# odchylenie startadowe powierzchni działki dla kodu:  98103 
# [1] 1832.009
# [1] "najpopularniejsza liczba łazieniek"
# [1] 1
# odchylenie startadowe powierzchni działki dla kodu:  98115 
# [1] 2675.302
# [1] "najpopularniejsza liczba łazieniek"
# [1] 1
# odchylenie startadowe powierzchni działki dla kodu:  98117 
# [1] 2318.662
# [1] "najpopularniejsza liczba łazieniek"
# [1] 1

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia 
# mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) 
# sqft, nieznajdujących się przy wodzie.

df %>% 
  filter(waterfront == 0 & sqft_living > 0 & sqft_living <= 2000) %>% 
  select(price) %>% 
  summary()
# Median : 359000  
# Mean   : 385084 

df %>% 
  filter(waterfront == 0 & sqft_living > 2000 & sqft_living <= 4000) %>% 
  select(price) %>% 
  summary()
# Median : 595000  
# Mean   : 645419 

df %>% 
  filter(waterfront == 0 & sqft_living > 4000) %>% 
  select(price) %>% 
  summary()
# Median :1262750  
# Mean   :1448119 

# Odp: Średnia i mediana ceny rośnie wraz z powierzchnią mieszkania


# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? 
# (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

v = df$price / df$sqft_living
min(v)

# Odp: 87.58824

