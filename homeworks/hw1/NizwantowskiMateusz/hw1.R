library(dplyr)

df <- read.csv("../house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

df %>%
  filter(bathrooms>median(bathrooms)) %>%
  filter(long>-122) %>%
  summarise(avg = mean(price))
  
# Odp:  625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>%
  count(yr_built) %>%
  top_n(1, n) %>%
  select(yr_built)

# Odp:  2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?

df %>%
  group_by(waterfront) %>%
  summarise(med = median(price))

(1400000/450000 - 1) * 100

# Odp: 211%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?

df %>%
  filter(floors == 1) %>%
  group_by(yr_built) %>%
  slice_min(order_by = price) %>%
  ungroup() %>%
  summarise(avg = mean(sqft_living))

# Odp: 1030

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

df %>% 
  filter(bathrooms %in% c(1,2)) %>%
  group_by(bathrooms) %>%
  summarise(pierwszy = quantile(grade,0.25), trzeci = quantile(grade,0.75))

# Odp: Jest roznica, detla_q1 = 1, delta_q2 = 1
#     bathrooms pierwszy trzeci

#             1        6      7
#             2        7      8

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)

center = (min(df$lat) + max(df$lat)) / 2 
df %>%
  mutate(is_north = case_when(lat <= center ~ 0,
                    TRUE ~ 1)) %>%
  group_by(is_north) %>%
  summarise(odstep = IQR(price))

# Odp: south = 122500, north = 321000

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?

df %>%
  filter(waterfront == 0) %>%
  mutate(sqft_per_lvl = sqft_living / floors) %>%
  filter(sqft_per_lvl < 1800) %>%
  count(bathrooms) %>%
  arrange(n)

# Odp: min = 4.75; max = 2.5 

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

df %>%
  count(zipcode) %>%
  filter(n > 550) %>%
  select(zipcode) -> ans

ans = ans$zipcode

df %>%
  filter(zipcode %in% ans) %>%
  group_by(zipcode) %>%
  summarise(odchylenie_standardowe = sd(sqft_lot), łazieneki = getmode(bathrooms))

# Odp:  zipcode odchylenie_standardowe łazieneki
#     1   98038                 63111.       2.5
#     2   98052                 10276.       2.5
#     3   98103                  1832.       1  
#     4   98115                  2675.       1  
#     5   98117                  2319.       1  

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

df %>%
  filter(waterfront == 0) %>%
  mutate(powierzchnia = case_when(sqft_living <= 2000 ~ "Male",
                                  sqft_living <= 4000 ~ "Srednie",
                                  TRUE ~ "Duze")) %>%
  group_by(powierzchnia) %>%
  summarise(srednia = mean(price), mediana = median(price))

# Odp:   powierzchnia  srednia mediana
#        Duze         1448119. 1262750
#        Male          385084.  359000
#        Srednie       645419.  595000

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df %>%
  mutate(cost_of_sqm = price / (0.09290304 * sqft_living)) %>%
  summarise(najtansze = min(cost_of_sqm))

# Odp:  942.7919

