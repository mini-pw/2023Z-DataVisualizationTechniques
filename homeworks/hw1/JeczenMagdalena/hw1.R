library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?
df$long
summary(df$long)

df %>% 
  summarise(median_bathrooms = median(bathrooms)) -> median_bathrooms
median_bathrooms <- as.numeric(median_bathrooms)

df %>% 
  filter(bathrooms > median_bathrooms & long > -122) %>% 
  summarise(mean_price = mean(price))

# Odp: 625499.4 USD

# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>% 
  group_by(yr_built) %>% 
  summarise(n = n()) %>% 
  top_n(1, n)

# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?

df %>% 
  group_by(waterfront) %>% 
  summarise(waterfront_median = median(price)) -> dif

100 - ((dif[1,2]*100)/ dif[2,2])

# Odp: 67.85714

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?

summary(df$floors)

df %>% 
  filter(floors == 1) %>% 
  group_by(yr_built) %>% 
  summarise(min_sqrt_living  = min(sqft_living)) %>% 
  summarise(mean_sqrt_living = mean(min_sqrt_living))

# Odp: 755

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

df %>% 
  filter(bathrooms == 1) -> b1
summary(b1$grade)

df %>% 
  filter(bathrooms == 2) -> b2
summary(b2$grade)

# Odp: q1 jak i q3 dla nieruchomości z jedną lazienką są o 1 mniejsze od q1 i q3 dla nieruchomosci z dwiema

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)

min(df$lat) + ((max(df$lat) - min(df$lat)))/2 -> avr

df %>% 
  filter(lat > avr) -> n

df %>% 
  filter(lat < avr) -> s

summary(n$price) -> n_s
summary(s$price) -> s_s

abs(n_s[2] - n_s[5])
abs(s_s[2] - s_s[5])

# Odp: Na północy: 321000, na południu: 122500 

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?

df %>% 
  filter(waterfront == 0) %>% 
  mutate(kon = sqft_living/floors)%>% 
  filter(kon <= 1800) %>% 
  group_by(bathrooms) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% 
  filter(row_number() == 1 | row_number() == n())


  

# Odp: Najczesciej: 2.5, najrzadziej: 4.75

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek

                  
df %>% 
  filter(zipcode %in% (df %>% 
                         group_by(zipcode) %>% 
                         summarise(n = n()) %>% 
                         filter(n > 550) %>% 
                         select(1) %>% 
                         pull())) %>% 
  group_by(zipcode) %>% 
  summarise(odch = sd(sqft_living)) %>% 
  arrange(zipcode)

df %>% 
  filter(zipcode %in% (df %>% 
                         group_by(zipcode) %>% 
                         summarise(n = n()) %>% 
                         filter(n > 550) %>% 
                         select(1) %>% 
                         pull())) %>% 
  group_by(zipcode) %>% 
  summarise(b = names(which.max(table(bathrooms)))) %>%
  arrange(zipcode)


# Odp: 98038: 691, 2.5; 98052: 745, 2.5; 98103: 632,1; 98115: 723, 1; 98117: 684,1

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

df %>%
  filter(waterfront == 0) %>%
  mutate(przedzial = case_when(sqft_living <= 2000 ~ "(0, 2000]",
                              sqft_living <= 4000 ~ "(2000,4000]",
                              TRUE ~ "(4000, +Inf)")) %>%
  group_by(przedzial) %>%
  summarise(mean = mean(price), median = median(price))

# Odp: Średnia1 < średnia 2 < średnia 3 oraz mediana1 < mediana2 < mediana3, gdzie 1,2,3 to przedział pierwszy, drugi i trzeci kolejno z polecenia.

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df %>% 
  mutate(price_sqft = price/sqft_living) %>% 
  arrange(desc(price_sqft)) %>% 
  head(1) %>% 
  select(price_sqft)

# Odp: 810.1389
