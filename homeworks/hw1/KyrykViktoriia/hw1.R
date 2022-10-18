# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?
df %>% 
  select(price, bedrooms,long) %>% 
  filter(bedrooms > median(bedrooms), long > -122) %>% 
  summarize(mean(price)) 

# Odp: 669744.9

# 2. W którym roku zbudowano najwięcej nieruchomości?
df %>% 
  count(yr_built) %>% 
  top_n(1) %>% 
  select(yr_built)

# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
above_water <- df %>% 
  filter(waterfront == 1) %>% 
  summarise(median(price, na.rm = T))

no_above_water <- df %>% 
  filter(waterfront == 0) %>% 
  summarise(median(price, na.rm = T))

how_bigger <- ((above_water - no_above_water) / above_water) * 100
how_bigger
  
# Odp: 67.85714%


# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
df %>% 
  select(sqft_living, yr_built,floors,price) %>% 
  filter(floors == 1) %>%
  group_by(yr_built) %>% 
  summarise(Min = min(price)) %>% 
  left_join(filter(df, floors == 1), by = c("yr_built"="yr_built", "Min"="price")) %>%
  summarise(Mean = mean(sqft_living))

# Odp: 1030


# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?
with_1_bath <- df %>% 
  filter(bathrooms == 1) %>% 
  summarise(grade_rank = quantile(grade,c(0.25,0.75))) %>% 
  summarise(difference = grade_rank[2]-grade_rank[1])
with_1_bath


with_2_bath <- df %>% 
  filter(bathrooms == 2) %>% 
  summarise(grade_rank = quantile(grade,c(0.25,0.75))) %>% 
  summarise(difference = grade_rank[2]-grade_rank[1])
with_2_bath

# Odp: Tak, jest różnica. Dla nieruchomości z jedną łazienką Q1 wynosi 6, a Q3 wynosi 7, różnica między nimi jest równa 1. Dla nieruchomości z jedną łazienką Q1 wynosi 7, a Q3 wynosi 8, różnica między nimi jest równa 1.


# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)
north <- df %>% 
  filter(lat > quantile(lat,c(0.5))) %>% 
  summarise(IQR(price))
north

south <- df %>% 
  filter(lat < quantile(lat,c(0.5))) %>% 
  summarise(IQR(price))
south

# Odp: Odstęp międzykwartylowy ceny mieszkań położonych na północy: 315000, odstęp międzykwartylowy ceny mieszkań położonych na południu: 218000.


# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
most_frequent <- df %>% 
  filter(waterfront == 0, sqft_living / floors <= 1800) %>% 
  count(bathrooms) %>% 
  slice(which.max(n)) %>% 
  select(-contains("n"))
most_frequent

most_rarely <- df %>% 
  filter(waterfront == 0, sqft_living / floors <= 1800) %>% 
  count(bathrooms) %>% 
  slice(which.min(n)) %>% 
  select(-contains("n"))
most_rarely

# Odp: Najczęściej: 2.5,  najrzadziej: 4.75


# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
standard_deviation <- df  %>% 
  group_by(zipcode) %>% 
  filter(n() > 550) %>% 
  summarise(st_dev = sd(sqft_lot))

number_of_bathrooms <- df %>% 
  group_by(zipcode) %>% 
  filter(n() > 550) %>% 
  count(bathrooms) %>%
  slice(num_bathrooms = which.max(n)) %>% 
  select(-contains("n"))
  
res <- inner_join(standard_deviation, number_of_bathrooms, by="zipcode")
View(res)

# Odp: kod pocztowy: 98038, odchylenie standardowe powierzchni działki: 63111.112, najpopularniejsza liczba łazienek: 2.5;
#      kod pocztowy: 98052, odchylenie standardowe powierzchni działki: 10276.188, najpopularniejsza liczba łazienek: 2.5;
#      kod pocztowy: 98103, odchylenie standardowe powierzchni działki: 1832.009, najpopularniejsza liczba łazienek: 1.0;
#      kod pocztowy: 98115, odchylenie standardowe powierzchni działki: 2675.302, najpopularniejsza liczba łazienek: 1.0;
#      kod pocztowy: 98117, odchylenie standardowe powierzchni działki: 2318.662, najpopularniejsza liczba łazienek: 1.0;


# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.
przedzial_1 <- df %>% 
  filter(sqft_living <= 2000, waterfront == 0) %>% 
  summarise(Mean = mean(price), Median = median(price)) %>% 
  mutate(difference = Mean-Median)
przedzial_1

przedzial_2 <- df %>% 
  filter((sqft_living <= 4000 & sqft_living > 2000), waterfront == 0) %>% 
  summarise(Mean = mean(price), Median = median(price)) %>% 
  mutate(difference = Mean-Median)
przedzial_2  

przedzial_3 <- df %>% 
  filter((sqft_living < Inf & sqft_living > 4000), waterfront == 0) %>% 
  summarise(Mean = mean(price), Median = median(price)) %>% 
  mutate(difference = Mean-Median)
przedzial_3

# Odp: Dla predziału (0, 2000] średnia jest większa od mediany na 26084.32, dla predziału (2000,4000] średnia jest większa od mediany na 50419.04, dla predziału (4000, +Inf) średnia jest większa od mediany na 185368.8.


# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
df %>% 
  transmute(pricee = price/(sqft_living*0.09290304)) %>% 
  summarise(min = min(pricee))

# Odp: 942.7919
