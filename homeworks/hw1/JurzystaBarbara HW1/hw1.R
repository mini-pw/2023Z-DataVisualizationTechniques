library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

df %>% 
  summarise(median_bathrooms = median(bathrooms)) -> median_bathrooms

filter(df, bathrooms > as.numeric(median_bathrooms) ) %>% filter(long>-122) %>% summarise(mean_price = mean(price)) -> df1

# Odp: 625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>% count(yr_built) %>% arrange(-n) %>% head(1) %>% select(yr_built) -> df2

# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
  
df %>% group_by(waterfront) %>% summarise(median_price = median(price)) -> df3
(as.numeric(df3[2,2])-as.numeric(df3[1,2]))/as.numeric(df3[2,2])*100 

# Odp: 67.85714 %

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?


df %>% filter(floors == 1) %>% group_by(yr_built) %>% summarise(min_price = min(price)) -> min
left_join( min, df, by = c("min_price"="price","yr_built" = "yr_built")) %>% filter(floors == 1) -> min
# w roku 1971 są dwa rekordy z najtańszymi nieruchomościami(mają taką samą cene) posiadającymi jedno piętro, zdecydowałam się do średniej liczyć oba
min %>% select(sqft_living) %>% summarise(mean_sqft_living = mean(sqft_living)) -> df4
as.numeric(df4)
# Odp: 1030.422

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?
df %>% filter(bathrooms == 1| bathrooms == 2) %>% group_by(bathrooms) %>% summarise(quantile = quantile(grade)) ->df5
difference_onebathroom = as.numeric(df5[4,2])-as.numeric(df5[2,2])
difference_twobathrooms = as.numeric(df5[9,2])-as.numeric(df5[7,2])
# Odp:
# Tak jest różnica
# dla nieruchomości z 1 łazienką Q1 = 6, a Q3 = 7, różnica wynosi 1
# dla nieruchomości z 2 łazienkami Q1 = 7, a Q3 = 8, różnica wynosi 1

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)
df %>% select(lat) %>% arrange(lat) %>% head(1) -> lat1
df %>% select(lat) %>% arrange(-lat) %>% head(1) -> lat2
lat6 = (as.numeric(lat1)+as.numeric(lat2))/2
df %>% select(price,lat) %>% mutate(north_or_south = ifelse(lat > lat6, "north", "south")) %>% group_by(north_or_south) %>% summarise(quantile = quantile(price)) -> df6

difference_north = as.numeric(df6[4,2])-as.numeric(df6[2,2])
difference_south = as.numeric(df6[9,2])-as.numeric(df6[7,2])
# Odp:
# odstęp międzykwartylowy cen mieszkań na północy = 321000
# odstęp międzykwartylowy cen mieszkań na południu = 122500


# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
df %>% filter(waterfront == 0) %>% mutate(sqftforfloors = sqft_living/floors) %>% filter(sqftforfloors<=1800) %>% group_by(bathrooms) %>% summarise(bathrooms1 = n()) %>% arrange(bathrooms1)-> df7
df7 %>% head(1) ->df7min
df7 %>% arrange(-bathrooms1) %>% head(1) -> df7max


# Odp: najczęściej - 2.5, najrzadziej - 4.75

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
df %>% group_by(zipcode) %>% summarise(count = n()) %>% filter(count > 550) -> df8
left_join(df8,df,by =c("zipcode"="zipcode") ) %>% group_by(zipcode) %>% summarise(sd = sd(sqft_lot)) -> df8a
left_join(df8,df,by =c("zipcode"="zipcode") ) %>% group_by(zipcode,bathrooms) %>% summarise(bathrooms1 = n()) -> df8b
df8b %>% group_by(zipcode) %>% summarise(bathrooms2 = max(bathrooms1)) -> df8c
left_join(df8c,df8b,by = c("zipcode"="zipcode","bathrooms2" = "bathrooms1")) %>% select(1,3)-> df8d
inner_join(df8a,df8d,by=c("zipcode"="zipcode")) ->df8

# Odp:
# dla kodu pocztowego: 98038, odchylenie standardowe powierzchni działki wynosi: 63111.112, a najpopularniejsza liczba łazienek to: 2.5
# dla kodu pocztowego: 98052, odchylenie standardowe powierzchni działki wynosi: 10276.188, a najpopularniejsza liczba łazienek to: 2.5
# dla kodu pocztowego: 98103, odchylenie standardowe powierzchni działki wynosi: 1832.009, a najpopularniejsza liczba łazienek to: 1
# dla kodu pocztowego: 98115, odchylenie standardowe powierzchni działki wynosi: 2675.302, a najpopularniejsza liczba łazienek to: 1
# dla kodu pocztowego: 98117, odchylenie standardowe powierzchni działki wynosi: 2318.662, a najpopularniejsza liczba łazienek to: 1


# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.


df %>% filter(waterfront == 0) %>% mutate(price_i = ifelse(sqft_living <= 2000, "low", ifelse(sqft_living > 2000 & sqft_living <= 4000, "medium", "high"))) %>% group_by(price_i) %>% summarize(mean = mean(price), meadian = median(price)) -> df9
df9 %>% mutate(difference = df9$mean - df9$meadian) ->df9
 # Odp:
#dla powierzchni (0, 2000] średnia cena = 385084.3 mediana ceny = 359000  średnia>mediany o 26084.32
#dla powierzchni (2000,4000] średnia cena = 645419.0 mediana ceny = 595000  średnia>mediany o 50419.04
#dla powierzchni (4000, +Inf) średnia cena = 1448118.8 mediana ceny = 1262750 średnia>mediany o 185368.75

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df %>% mutate(sqm_living = sqft_living/10.76391041671) %>% mutate(price_sqm = price/sqm_living) %>% select(price_sqm) %>% arrange(price_sqm)%>% head(1) -> df10
as.numeric(df10)

# Odp: 942.7919

