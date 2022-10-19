library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

df %>%
  filter(bathrooms > median(bathrooms), long > -122) %>%
  summarise(mean_price = mean(price))
  
# Odp: 625499.4


# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>%
  group_by(substring(date,1,4)) %>%
  count

# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?

tmp <- df %>%
  group_by(waterfront) %>%
  summarise(mediana = median(price)) 
tmp$mediana[2] - tmp$mediana[1]

# Odp: 950000

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?

tmp <- df %>%
  filter(floors == 1) %>%
  group_by(substring(date,1,4)) %>%
  arrange(price) %>%
  head(2)
mean(tmp$sqft_living)

# Odp: 725

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

tmp1 <- df %>%
  filter(bathrooms == 1)
tmp2 <- df %>%
  filter(bathrooms == 2)

quantile(tmp1$grade, probs = c(0.25, 0.75)) - 
  quantile(tmp2$grade, probs = c(0.25, 0.75))

# Odp: Nieruchomości z dwoma łazienkami mają jakość wyższą o 1 dla Q1 oraz Q3

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)

midpoint = (max(df$lat) + min(df$lat)) / 2
tmp1 <- df %>%
  filter(lat > midpoint)
tmp2 <- df %>%
  filter(lat < midpoint)
tmp1 <- quantile(tmp1$price, probs = c(0.25, 0.75))
tmp2 <- quantile(tmp2$price, probs = c(0.25, 0.75))
tmp1[2] - tmp1[1]
tmp2[2] - tmp2[1]

# Odp: Dla północy: 321000, dla południa: 122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?

tmp <- df %>%
  filter(waterfront == 0) %>%
  filter((sqft_living/floors) <= 1800) %>%
  group_by(bathrooms) %>%
  summarise(n = n()) %>%
  arrange(n)
head(tmp, 1)
tail(tmp, 1)

# Odp: najczęsciej 2.5 łazienki, najrzadziej 4.75 łazienki

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek

tmp <- df %>%
  group_by(zipcode) %>%
  summarise(n = n(), sd = sd(sqft_lot)) %>%
  filter(n > 550) %>%
  select(zipcode, sd)

for ( i in tmp$zipcode){
  print(i)
  df %>%
    filter(zipcode == i) %>%
    group_by(bathrooms) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    head(1) %>%
    print()
}

# Odp:  code:98038, sd:63111.112, bathrooms:2,5
#       code:98052, sd:10276.188, bathrooms:2,5
#       code:98103, sd:1832.009, bathrooms:1
#       code:98115, sd:2675.302, bathrooms:1
#       code:98117, sd:2318.662, bathrooms:1


# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

df %>%
  filter(waterfront == 0, sqft_living > 0, sqft_living <= 2000) %>%
  summarise(mean(price), median(price))

df %>%
  filter(waterfront == 0, sqft_living > 2000, sqft_living <= 4000) %>%
  summarise(mean(price), median(price))

df %>%
  filter(waterfront == 0, sqft_living > 4000) %>%
  summarise(mean(price), median(price))

# Odp:  dla przedziału (0, 2000]    średnia:385084.3 mediana:359000
#       dla przedziału (2000, 4000] średnia:645419   mediana:595000
#       dla przedziału (4000, +Inf] średnia:1448119  mediana:1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df %>%
  mutate(sqm_living = sqft_living*(0.3048^2),
         price_sqm = price/sqm_living) %>%
  slice_min(price_sqm) %>%
  select(price_sqm)

# Odp: 942.7919

