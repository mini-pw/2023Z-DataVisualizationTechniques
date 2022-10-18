# Marta Szuwarska - HW1
library(dplyr)

df <- read.csv("house_data.csv")
df

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

View(df)

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?
Mediana <-  unlist(df %>% 
    summarise(mediana = median(bathrooms)))

df %>% 
  filter(bathrooms > Mediana, long > -122) %>% 
  summarise(srednia = mean(price))

# Odp: 625499.4



# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>% 
  group_by(yr_built) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(1)

# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
results <- {
  df %>% 
  group_by(waterfront) %>% 
  summarise(mediana = median(price)) %>% 
  arrange(-mediana)
}
(results[[1,2]]-results[[2,2]])/results[[2,2]]*100
# Odp: 211.1111%


# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
df %>% 
  filter(floors == 1) %>% 
  group_by(yr_built) %>% 
  slice_min(price) %>% 
  ungroup %>% 
  summarise(srednia = mean(sqft_living))
    

# Odp: 1030.

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?
df %>% 
    filter(bathrooms %in% c(1,2)) %>%
    group_by(bathrooms) %>% 
    summarise(Q1 = quantile(grade,0.25),Q3=quantile(grade,0.75))

# Odp: Dla nieruchomości z jedną łazienką: Q1 = 6, Q3 = 7; dla nieruchomości z dwoma łazienkami: Q1 = 7, Q3 = 8.

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)
srodek <- mean(c(min(df$lat),max(df$lat)))
df %>% 
  mutate(polozenie = case_when(lat > srodek ~ "Polnoc", TRUE ~ "Poludnie")) %>% 
  group_by(polozenie) %>% 
  summarise(odstep = IQR(price))

# Odp: Dla mieszkań położonych na północy odstęp międzykwartylowy wynosi 321000, a dla tych na południu 122500 USD.

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
new_df <- {
  df %>% 
  filter(waterfront == 0, sqft_living/floors <= 1800) %>% 
  group_by(bathrooms) %>% 
  summarise(count = n()) %>% 
  arrange(count)
}
head(new_df,1)
tail(new_df,1)
  

# Odp: Najczęściej: 2.5, najrzadziej: 4.75.

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
zipcodes <- unlist(
  df %>% 
  group_by(zipcode) %>% 
  summarise(count = n()) %>% 
  filter(count > 550) %>% 
  select(zipcode)
)
dominanta <- function(x) unique(x)[which.max(tabulate(match(x, unique(x))))]

df %>% 
  filter(zipcode %in% zipcodes) %>% 
  group_by(zipcode) %>% 
  summarise(odchylStand = sd(sqft_lot), najpopLiczbaLaz = dominanta(bathrooms))
# Odp:
# zipcode odchylStand najpopLiczbaLaz
# <int>       <dbl>           <dbl>
#   1   98038      63111.             2.5
# 2   98052      10276.             2.5
# 3   98103       1832.             1  
# 4   98115       2675.             1  
# 5   98117       2319.             1 

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.
df %>% 
  filter(waterfront == 0) %>% 
  mutate(sqft_interval = case_when(sqft_living <= 2000 ~ "(0, 2000]", sqft_living <= 4000 ~ "(2000,4000]", TRUE ~ "(4000, +Inf)")) %>% 
  group_by(sqft_interval) %>% 
  summarise(srednia = mean(price), mediana = median(price))

# Odp:
# sqft_interval  srednia mediana
# <chr>            <dbl>   <dbl>
#   1 (0, 2000]      385084.  359000
# 2 (2000,4000]    645419.  595000
# 3 (4000, +Inf)  1448119. 1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
df %>% 
  mutate(cenaZaMetr = price/sqft_living*(3.280839895)^2) %>% 
  select(cenaZaMetr) %>% 
  arrange(cenaZaMetr) %>% 
  head(1)

# Odp: 942.7919 USD

