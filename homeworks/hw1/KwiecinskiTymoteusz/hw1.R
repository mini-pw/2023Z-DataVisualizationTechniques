library(dplyr)


df <- read.csv("homeworks/hw1/house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie


head(df)
# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

df %>% 
  filter(bathrooms > median(bathrooms), long > -122) %>% 
  summarize(srednia_cena = mean(price)) -> srednia_cena

# Odp:
srednia_cena
# 625499.4 dolary


# 2. W którym roku zbudowano najwięcej nieruchomości?
df %>% 
  group_by(yr_built) %>% 
  summarize(n = n()) %>% 
  top_n(1, n) %>% 
  select(yr_built) -> rok

# Odp:
rok
# 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
df %>% 
  group_by(waterfront) %>% 
  summarize(mediana_ceny = median(price)) %>% 
  mutate(procent_ceny = (mediana_ceny/min(mediana_ceny) - 1)*100) %>% 
  select(procent_ceny) %>% 
  top_n(1, procent_ceny) -> procent_ceny

# Odp:
procent_ceny

# o 211%


# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
df %>% 
  filter(floors == 1) %>% 
  group_by(yr_built) %>% 
  filter(price == min(price)) %>% 
  summarize(avg_lot = sqft_lot) -> wedlug_roku

df %>%
  filter(floors == 1) %>% 
  group_by(yr_built) %>% 
  filter(price == min(price)) %>% 
  select(sqft_living) %>% 
  ungroup() %>%
  summarize(mean(sqft_living)) -> srednia_powierzchnia


# Odp:
srednia_powierzchnia
# 1030 stop kwadratowych


# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

quartiles <- c(.25, .75)

df %>% 
  filter(bathrooms == 1 | bathrooms == 2) %>%
  group_by(bathrooms) %>% 
  summarize(quartile=scales::percent(quartiles), val = quantile(grade, quartiles))


# Odp:

# tak jest roznica, roznia sie o 1 w obu kategoriach - dla łazienek z jedną łazienką 
# Q1 = 6, Q3 = 7, dla dwóch łazienek Q1 = 7, Q3 = 8

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)

srodek <- (max(df$lat) + min(df$lat)) / 2

df %>% 
  mutate(pnpd = ifelse(lat <= srodek, "poludnie", "polnoc")) %>% 
  group_by(pnpd) %>% 
  summarise(rozstep = IQR(price))

# Odp:
# dla nieruchomosci położonych na poludnie rozstęp wynosi 122500
# na polnoc 321000

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?

df %>% 
  filter(waterfront == 0, sqft_living/floors <= 1800) %>% 
  group_by(bathrooms) %>% 
  summarize(n = n()) %>% 
  arrange(n) -> bathrooms

bathrooms %>% top_n(1)
bathrooms %>% top_n(-1)


# Odp:

# najwiecej jest 2.5 lazienek
# najmniej 4.75 lazienek

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek

mode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

  
df %>% 
  group_by(zipcode) %>% 
  summarise(n = n()) %>% 
  filter(n >= 550) -> to_select

df %>% 
  inner_join(to_select, by="zipcode") %>% 
  group_by(zipcode) %>% 
  summarise(odchylenie = sd(sqft_lot), najpopularniejsze = mode(bathrooms))

# Odp:


# zipcode odchylenie najpopularniejsze
# 1   98038     63111.               2.5
# 2   98052     10276.               2.5
# 3   98103      1832.               1  
# 4   98115      2675.               1  
# 5   98117      2319.               1

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

df %>% 
  filter(waterfront == 0) %>% 
  mutate( pow = case_when(
    sqft_living <= 2000 ~ "(0, 2000]",
    sqft_living <= 4000 ~ "(2000, 4000]",
    TRUE ~ "(4000, Inf)"
  )) %>% 
  group_by(pow) %>% 
  summarize(srednia = mean(price), mediana = median(price))
  

# Odp:

# pow           srednia mediana
# 1 (0, 2000]     385084.  359000
# 2 (2000, 4000]  645419.  595000
# 3 (4000, Inf)  1448119. 1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df %>% 
  mutate(sqm_living = sqft_living * 0.09290304) %>% 
  mutate(cena_za_metr = price / sqm_living) %>% 
  top_n(cena_za_metr, n = -1) %>% 
  select(cena_za_metr)
# Odp:

# 942.7919 dolarów za metr kwadratowy
