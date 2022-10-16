library(dplyr)
library(stringi)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

df %>% 
  filter(bathrooms > median(df$bathrooms) & long > -122) %>% 
  summarise(mean_price = mean(price))

# Odp: 625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>% 
  group_by(yr_built) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)

# Odp: 2014  

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
med_water = median(filter(df, waterfront == 1)$price)
med = median(filter(df, waterfront == 0)$price)
(med_water - med) / med * 100

# Odp: 211.1111

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?

df %>% 
  filter(floors == 1) %>% 
  group_by(yr_built) %>% 
  summarise(price_min = min(price)) -> res

left_join(res, df[c("sqft_living", "yr_built", "price")], by = c("yr_built", "price_min" = "price")) %>% 
  summarise(mean_sqft_living = mean(sqft_living))

# Odp: 1062.

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

df %>% 
  filter(bathrooms == 1) %>% 
  select(grade) %>%
  arrange(grade) -> one_bathroom
df %>% 
  filter(bathrooms == 2) %>% 
  select(grade) %>% 
  arrange(grade) -> two_bathrooms
q1_one <- median(one_bathroom[c(1:(nrow(one_bathroom) / 2)),])
q3_one <- median(one_bathroom[c((nrow(one_bathroom) / 2):nrow(one_bathroom)),])

q1_two <- median(two_bathrooms[c(1:(nrow(two_bathrooms) / 2)),])
q3_two <- median(two_bathrooms[c((nrow(two_bathrooms) / 2):nrow(two_bathrooms)),])

(q1_one - q1_two) != 0 & (q3_one - q3_two) != 0

q1_one - q1_two
q3_one - q3_two

# Odp: TRUE \n -1 \n -1

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)

middle <- median(df$lat)
df %>% 
  filter(lat > middle) %>% 
  select(price) %>% 
  arrange(price) -> n

df %>% 
  filter(lat < middle) %>% 
  select(price) %>% 
  arrange(price) -> s

q1_n <- median(n[c(1:(nrow(n) / 2)),])
q3_n <- median(n[c((nrow(n) / 2):nrow(n)),])
q3_n - q1_n

q1_s <- median(s[c(1:(nrow(s) / 2)),])
q3_s <- median(s[c((nrow(s) / 2):nrow(s)),])
q3_s - q1_s

# Odp: 315000 \n 217840

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?

df %>% 
  filter(waterfront == 0 & sqft_living / floors <= 1800) %>% 
  group_by(bathrooms) %>% 
  summarise(n = n()) %>% 
  arrange(n) -> res

tail(res, 1)
head(res, 1)

# Odp: 2.5 \n 4.75

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek

df %>% 
  group_by(zipcode) %>% 
  summarise(n = n()) %>% 
  filter(n > 550) -> res

res = res$zipcode

df %>% 
  filter(zipcode %in% res) -> res

getmode <- function(v) {
  sort(unique(v))[which.max(table(v))]
}

res %>% 
  group_by(zipcode) %>% 
  summarise(odch_st = sd(sqft_lot), mode = getmode(bathrooms))

# Odp:
#   zipcode odch_st  mode
#   98038  63111.   2.5
#   98052  10276.   2.5
#   98103   1832.   1  
#   98115   2675.   1  
#   98117   2319.   1

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

df %>% 
  filter(waterfront == 0 & sqft_living > 0) %>% 
  mutate(sqft_living_groups = ifelse(sqft_living <= 2000, "(0, 2000]", ifelse(sqft_living <= 4000, "(2000,4000]", "(4000, +Inf)"))) %>% 
  group_by(sqft_living_groups) %>% 
  summarise(mean_price = mean(price), median_price = median(price))

  
# Odp:
#   sqft_living_groups mean_price median_price
#1 (0, 2000]             385084.       359000
#2 (2000,4000]           645419.       595000
#3 (4000, +Inf)         1448119.      1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df %>% 
  mutate(sqm_living = 0.3048 ** 2 * sqft_living) %>% 
  mutate(price_per_m = price / sqm_living) %>% 
  arrange(price_per_m) %>% 
  head(1) %>% 
  select(price_per_m)

# Odp: 942.7919
