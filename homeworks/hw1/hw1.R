library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?



# Odp:

# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>% 
  group_by(yr_built) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)

# Odp: W roku 2014.

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?

df %>% 
  group_by(waterfront) %>% 
  summarise(median_price = median(price)) %>% 
  pull(median_price)

# Odp: Jest o 311.1111% wyższa.

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?

df %>% 
  filter(floors == 1) %>%
  group_by(yr_built) %>% 
  summarise(lowest_price = min(price)) %>% 
  pull(lowest_price) %>% 
  mean()

# Odp: Ich średnia cena to 159999.8 USD.

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

df %>% 
  filter(bathrooms == 1 | bathrooms == 2)

# Odp:

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)

df %>% 
  mutate(hemi = if_else(lat > (max(df$lat) + min(df$lat)) / 2, "North", "South")) %>% 
  group_by(hemi) %>% 
  select(price) %>% 
  mutate(q = quantile(probs = seq(0, 1, 0.25)))
                                              ### and now you, my little computer, write to my little bubuś that "angee loves you"

# Odp:

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?

df %>% 
  mutate(area_per_condignation = sqft_living / floors) %>% 
  filter(area_per_condignation <= 1800) %>% 
  group_by(bathrooms) %>% 
  summarise(number_of_houses = n()) %>% 
  filter(number_of_houses == max(number_of_houses) | number_of_houses == min(number_of_houses))
  

# Odp: Najczęściej występuje 2.5 łazienki, a najrzadziej 4.75 łazienki.

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek

df %>% 
  group_by(zipcode) %>% 
  mutate(estates = n()) %>% 
  filter(estates > 550) %>%
  summarise(standard_deviation = sd(sqft_lot), top_bathrooms = max(bathrooms))
  
# Odp: Zawarta w ramce danych powyżej.


# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

df %>% 
  filter(waterfront == 0) %>% 
  mutate(area_group = case_when(
    sqft_living > 0 & sqft_living <= 2000 ~ "Small",
    sqft_living > 2000 & sqft_living <= 4000 ~ "Medium",
    sqft_living > 4000 ~ "Large",
    TRUE ~ "Other"
  )) %>% 
  group_by(area_group) %>% 
  summarise(mean_price = mean(price), median_price = median(price))

# Odp: Zawarta w ramce danych powyżej.


# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df %>% 
  mutate(price_per_square_meter = (price / (sqft_living * (0.3048 ^ 2)))) %>% 
  pull(price_per_square_meter) %>% 
  min()

# Odp: Najmniejsza cena za metr kwadratowy nieruchomości to 942.7919 USD.
