library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?
bathrooms_median <- median(df$bathrooms)
res <- df %>% filter(bathrooms > bathrooms_median & long > -122 ) %>% summarise(Mean = mean(price))

# Odp:
#625499.4 USD

# 2. W którym roku zbudowano najwięcej nieruchomości?
df %>% group_by(yr_built) %>% summarise(Count = n()) %>% slice_max(Count)

# Odp:
#2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
water_median <- df %>% filter(waterfront == 1) %>% summarise(Median = median(price))
no_water_median <- df %>% filter(waterfront == 0) %>% summarise(Median = median(price))
res <- water_median / no_water_median * 100 - 100

# Odp:
# 211.1111%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
res <- df %>% filter(floors == 1) %>% group_by(yr_built) %>% slice_min(price) %>% ungroup() %>% summarise(Mean = mean(sqft_living))

# Odp:
#1030.422 sqft

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?
df %>% filter(bathrooms == 1) %>% summarise(val = quantile(grade, c(0.25, 0.75)), q=c(0.25, 0.75))
df %>% filter(bathrooms == 2) %>% summarise(val = quantile(grade, c(0.25, 0.75)), q=c(0.25, 0.75))

# Odp:
# Nieruchomości z 1 łażienką:
# Q1: 6
# Q3: 7
# Nieruchomośći z 2 łazienkami;
# Q1: 7
# Q3: 8
# Zatem zarówno gdy porównamy pierwsze kwartyle, jak i trzecie kwartyle, różnica wyniesie 1

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)
mid_lat <- (max(df$lat) + min(df$lat)) / 2
df %>% filter(lat > mid_lat) %>% summarise(val = quantile(price, c(0.25, 0.75)))
df %>% filter(lat < mid_lat) %>% summarise(val = quantile(price, c(0.25, 0.75)))


# Odp
# na północy odstęp międzykwartylowy wynosi: 321000 USD
# na południu: 122500 USD

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
tmp <- df %>% filter(waterfront == 0 & sqft_living / floors <= 1800) %>% select(bathrooms) %>% group_by(bathrooms) %>% summarise(Count = n())
tmp %>% slice_max(Count)
tmp %>% slice_min(Count)

# Odp:
# Najczesciej wystepuje 2.5 lazienki, najrzadziej 4.75

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
zipcodes <- df %>% group_by(zipcode) %>% summarise(Count = n()) %>% filter(Count > 550) %>% pull(zipcode)
# liczba lazienek
col1 <- df %>% filter(zipcode %in% zipcodes) %>% count(zipcode, bathrooms) %>% group_by(zipcode) %>% slice(which.max(n)) %>% select(zipcode, bathrooms)
# odchylenie standardowe
col2 <- df %>% filter(zipcode %in% zipcodes) %>% group_by(zipcode) %>% summarise(sd_area = sd(sqft_lot))
merge(col1, col2)

# Odp:
#zipcode bathrooms    sd_area
#   98038       2.5   63111.112
#   98052       2.5   10276.188
#   98103       1.0   1832.009
#   98115       1.0   2675.302
#   98117       1.0   2318.662

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.
df %>% filter(waterfront == 0) %>% group_by(group=cut(sqft_living, breaks=c(0, 2000, 4000, Inf), dig.lab=10)) %>% summarise(Mean = mean(price), Median = median(price))
  
# Odp:
#group           Mean  Median
#(0,2000]     385084.  359000
#(2000,4000]  645419.  595000
#(4000,Inf]  1448119. 1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
df %>% mutate(price_per_sqft = price / sqft_living) %>% slice_min(price_per_sqft) %>% select(price_per_sqft) %>% mutate(price_per_sqm = price_per_sqft * 10.764)

# Odp:
# 942.7998 USD

