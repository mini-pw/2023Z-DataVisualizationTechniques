library(dplyr)

df <- read.csv("house_data.csv")


# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

df %>% 
  filter(bathrooms > median(bathrooms), long > -122) %>% 
  summarise(mean = mean(price))

# Odp: 625499.4


# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>% 
  group_by(yr_built) %>% 
  summarise(Total = n()) %>% 
  top_n(1, Total) %>% 
  select(yr_built)

# Odp: 2014


# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?

result <- df %>% 
  group_by(waterfront) %>%
  summarise(median = median(price))

(result[result$waterfront == 1, "median", drop = TRUE] / 
    result[result$waterfront == 0, "median", drop = TRUE] - 1 ) * 100

# Odp: o 211.1111%


# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?

result <- df %>% 
  filter(floors == 1) %>% 
  group_by(yr_built) %>% 
  filter(price == min(price))

mean(result$sqft_living)

# Odp: 1030.422


# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

result <- df %>% 
  filter(bathrooms == 1 | bathrooms == 2) %>% 
  group_by(bathrooms) %>% 
  summarise(Q1 = quantile(grade, 0.25), Q3 = quantile(grade, 0.75))

result[result$bathrooms == 2, c("Q1", "Q3")] - result[result$bathrooms == 1, c("Q1", "Q3")]

# Odp: oba kwartyle dla nieruchomości o dwóch łazienkach są o jeden większe od kwartyli dla nieruchomości 
#      o jednej łazience


# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)

df %>% 
  mutate(położenie = ifelse(lat > (max(lat) + min(lat))/2, "Północ", "Południe")) %>% 
  group_by(położenie) %>% 
  summarise(IQR = IQR(price))

# Odp: południe - 122500, północ - 321000


# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, 
# których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?

df %>% 
  filter(sqft_living/floors <= 1800, waterfront == 0) %>% 
  group_by(bathrooms) %>% 
  summarise(count = n()) %>%
  summarise(
    najczęściej = sum(case_when(
      count == max(count) ~ bathrooms,
      TRUE ~ 0)),
    najrzadziej = sum(case_when(
      count == min(count) ~ bathrooms,
      TRUE ~ 0))) 

# Odp: najczęściej 2.5, najrzadziej 4.75


# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek

df %>% 
  group_by(zipcode) %>% 
  filter(n() > 550) %>% 
  mutate(sd = sd(sqft_lot)) %>% 
  group_by(zipcode, bathrooms) %>% 
  summarise(sd = median(sd), count = n(), .groups = "drop_last") %>% 
  filter(count == max(count)) %>%
  ungroup(zipcode) %>% 
  select(zipcode, bathrooms, sd)

# Odp:
#   zipcode bathrooms     sd
#     <int>     <dbl>  <dbl>
# 1   98038       2.5 63111.
# 2   98052       2.5 10276.
# 3   98103       1    1832.
# 4   98115       1    2675.
# 5   98117       1    2319.


# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

df %>% 
  filter(waterfront == 0) %>% 
  mutate(powierzchnia = case_when(
    sqft_living <= 2000 ~ "(0, 2000]",
    sqft_living > 2000 & sqft_living <= 4000 ~ "(2000, 4000]",
    TRUE ~ "(4000, +Inf)"
  )) %>% 
  group_by(powierzchnia) %>% 
  summarise(mediana = median(price), srednia = mean(price))
  
# Odp:
#   powierzchnia mediana  srednia
#   <chr>          <dbl>    <dbl>
# 1 (0, 2000]     359000  385084.
# 2 (2000, 4000]  595000  645419.
# 3 (4000, +Inf) 1262750 1448119.


# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df %>% 
  mutate(powierzchnia_m2 = sqft_living * (0.3048 ** 2)) %>%   # zamiana ze stóp na metry
  mutate(cena_za_m2 = price/powierzchnia_m2) %>% 
  summarise(min = min(cena_za_m2))

# Odp: 942.7919

