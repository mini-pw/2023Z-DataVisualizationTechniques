library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

df %>% 
  filter(long > -122 & bathrooms > median(bathrooms)) %>% 
  summarise(mean_price = mean(price))
# Odp: 625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>% 
  group_by(yr_built) %>% 
  summarise(n = n()) %>% 
  top_n(1, n)
# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?

df %>% 
  group_by(waterfront) %>% 
  summarise(median_price = median(price)) %>% 
  mutate(percentage = if_else(waterfront == 0,
                       100,
                       median_price[waterfront == 1] * 100 / median_price[waterfront == 0]- 100))
# Odp: Jest wieksza o 211 procent

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?

df %>% 
  select(yr_built, price, sqft_living, floors) %>% 
  filter(floors == 1) %>% 
  group_by(yr_built) %>% 
  filter(price == min(price)) %>% 
  ungroup() %>% 
  summarise(mean_sqft = mean(sqft_living))
# Odp: 1030

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

df %>% 
  select(bathrooms, grade) %>% 
  filter(bathrooms == 1 | bathrooms == 2) %>% 
  group_by(bathrooms) %>% 
  summarise(grade = quantile(grade, c(0.25, 0.5, 0.75)), q = c(0.25, 0.5, 0.75))
# Odp: Roznica w przypadku zarowno pierwszego kwartyla, jak i trzeciego jest rowna 1 (nieruchomosci z dwoma lazienkami maja wieksza wartosc o 1)

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)

df %>%
  select(price, lat) %>% 
  mutate(dir = if_else(lat > (max(lat) + min(lat))/2,
                       "north",
                       "south")) %>% 
  group_by(dir) %>% 
  summarise(price =IQR(price))
# Odp:
# odstep miedzykwartylowey na polnocy: 321000
# odstep miedzykwartylowy na poludniu: 122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?

df %>% 
  filter(waterfront == 0 & sqft_living <= 1800) %>% 
  group_by(bathrooms) %>% 
  summarise(n = n()) %>% 
  summarise(min_bathroom = bathrooms[which.min(n)],
            max_bathroom = bathrooms[which.max(n)])
# Odp: Najczesciej wystepuje 1 lazienka, a najrzadziej 3.75.

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek

df %>% 
  select(zipcode, sqft_lot, bathrooms) %>% 
  group_by(zipcode) %>% 
  mutate(zip_n = n()) %>% 
  filter(zip_n > 550) %>% 
  summarise(sd_sqft = sd(sqft_lot),
            common_bath = names(which.max(table(bathrooms))))
# Odp:
# 98038 odchylenie standardowe powierzchni: 63111 najpopularniejsza liczba lazienek: 2.5
# 98052 odchylenie standardowe powierzchni: 10276 najpopularniejsza liczba lazienek: 2.5
# 98103 odchylenie standardowe powierzchni: 1832 najpopularniejsza liczba lazienek: 1
# 98115 odchylenie standardowe powierzchni: 2675 najpopularniejsza liczba lazienek: 1
# 98117 odchylenie standardowe powierzchni: 2319 najpopularniejsza liczba lazienek: 1

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

df %>% 
  select(price, sqft_living, waterfront) %>% 
  filter(waterfront == 0) %>% 
  mutate(sqft_living_groups = case_when(sqft_living <= 2000 ~ "(0, 2000]",
                                        sqft_living <= 4000 ~ "(2000, 4000]",
                                        TRUE ~ "(4000, +Inf)")) %>% 
  group_by(sqft_living_groups) %>% 
  summarise(mean_price = mean(price), median_price = median(price))
  
# Odp:
# (0, 2000] srednia: 385084 mediana: 359000
# (2000, 4000] srednia: 645419 mediana: 595000
# (4000, +Inf) srednia: 1448119 mediana: 1262750
# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df %>% 
  mutate(sqm_living = sqft_living * 0.09290304) %>%
  transmute(price_sqm = price / sqm_living) %>% 
  top_n(1, -price_sqm)
# Odp: 942.7919

