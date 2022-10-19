library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i 
# położonych na wschód od południka 122W?

df %>%
  filter(bathrooms > median(bathrooms)) %>%
  filter(long > -122) %>%
  summarise(meanprice = mean(price))

# Odp: 625499.4 USD

# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>%
  group_by(yr_built) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(1)

# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w 
# porównaniu z tymi położonymi nie nad wodą?

df %>%
  group_by(waterfront) %>%
  summarise(medianprice = median(price)) %>%
  mutate(difference = if_else(waterfront == 1, (medianprice[waterfront == 1] / 
                                medianprice[waterfront == 0]) * 100 - 100, 0))
  
# Odp: 211% większa

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych 
# nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym 
# roku?

df %>%
  filter(floors == 1) %>%
  group_by(yr_built) %>%
  slice_min(price) %>%
  ungroup() %>%
  summarise(mean_area = mean(sqft_living))

# Odp: 1030 sqft

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości 
# wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? 
# Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

df %>%
  filter(bathrooms == 1 | bathrooms == 2) %>%
  group_by(bathrooms) %>%
  summarise(grade_q = quantile(grade, c(0.25, 0.75)), q = c('Q1', 'Q3'))

# Odp: 1 Łazienka: Q1 = 6, Q3 = 7
# 2 Łazienki: Q1 = 7, Q3 = 8
# Różnice zarówno dla Q1 jak i Q3 wynoszą 1.

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a 
# jaki tych na południu? (Północ i południe definiujemy jako położenie 
# odpowiednio powyżej i poniżej punktu znajdującego się w połowie między 
# najmniejszą i największą szerokością geograficzną w zbiorze danych)

df %>%
  mutate(place = ifelse(lat > mean(c(max(lat), min(lat))), "north", "south")) %>%
  group_by(place) %>%
  summarise(iqr = IQR(price))

# Odp: Połnoc: 321000, Południe: 122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach
# niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie 
# przekracza 1800 sqft?

df %>%
  filter(waterfront == 0) %>%
  mutate(sqft_floor = sqft_living / floors) %>%
  filter(sqft_floor <= 1800) %>%
  group_by(bathrooms) %>%
  summarise(n = n()) %>%
  arrange(n) %>%
  filter(row_number() == 1 | row_number() == n())

# Odp: najrzadziej: 4.75, najczęściej 2.5

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. 
# Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz 
# najpopularniejszą liczbę łazienek

df %>%
  group_by(zipcode) %>%
  mutate(n = n()) %>%
  summarise(n = n(), sd_area = sd(sqft_lot), 
            mode = which.max(tabulate(bathrooms))) %>%
  filter(n > 550) %>%
  select(-n)

# Odp: kod pocztowy: 98038, odchylenie st.: 63111, łazienki: 2
# kp: 98052, os: 10276, ł: 2
# kp: 98103, os: 1832, ł: 1
# kp: 98115, os: 2675, ł: 1
# kp: 98117, os: 2319, ł: 1

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia 
# mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf)
# sqft, nieznajdujących się przy wodzie.

df %>%
  filter(waterfront == 0) %>%
  mutate(area = case_when(sqft_living <= 2000 ~ "1",
                          sqft_living <= 4000 ~ "2",
                          TRUE ~ "3")) %>%
  group_by(area) %>%
  summarise(mean_price = mean(price), median_price = median(price))
  
# Odp: (0, 2000]: średnia = 385084USD > mediana = 359000USD
# (2000,4000]: średnia = 645419USD > mediana = 595000USD
# (4000, +Inf): średnia = 1448119USD > mediana = 1262750USD

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? 
# (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df %>%
  mutate(sqm = sqft_living * 0.092903) %>%
  mutate(price_sqm = price / sqm) %>%
  arrange(price_sqm) %>%
  select(price_sqm) %>%
  head(1)

# Odp: 942.79 USD 
