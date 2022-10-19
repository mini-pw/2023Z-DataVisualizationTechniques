library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

df %>% 
  filter(bathrooms > median(df$bathrooms) & long > -122) %>% 
  pull(price) %>%
  mean()
  
# Odp: 625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>%
  mutate(year = substr(date, 1,4)) %>% 
  group_by(year) %>%
  summarise(n_sold=n()) %>%
  arrange(n_sold) %>%
  top_n(1)

# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?

df %>%
  group_by(waterfront) %>%
  summarise(med=median(price))
  (1400000-450000)/450000*100
  
# Odp: 211.11%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?

df %>%
  filter(floors == 1) %>%
  select(yr_built, price, sqft_living) -> prepared_data
df %>% 
  filter(floors == 1) %>%
  group_by(yr_built) %>%
  summarise(price = min(price)) %>% 
  inner_join(prepared_data) %>%
  summarise(mean(sqft_living))
# Odp:  1030

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

df %>%
  filter(bathrooms == 1) %>%
  summarise(val = quantile(grade, c(0.25, 0.75)), q = c("Q1", "Q3"))

df %>%
  filter(bathrooms == 2) %>%
  summarise(val = quantile(grade, c(0.25, 0.75)), Q_x = c("Q1", "Q3"))


# Odp: Q3 różni się o 1 od Q1 w obu przypadkach (7 i 6 oraz 8 i 7 odpowiednio)

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)

df %>%
  mutate(where = ifelse(lat >= (max(df$lat) + min(df$lat))/2,"N", "S")) %>%
  group_by(where) %>%
  summarise(iqr = IQR(price))

  
# Odp: N - 321000, S - 122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?

df %>%
  mutate(perfloor=sqft_living/floors) %>%
  filter(waterfront == 0 & perfloor <= 1800) %>%
  group_by(bathrooms) %>%
  summarise(number=n()) -> data
data %>%
  slice_max(number)
data %>%
  slice_min(number)
  
# Odp: najrzadziej 4.75, najczęściej 2.5

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek

df %>%
  group_by(zipcode) %>%
  summarise(number = n()) %>%
  right_join(df, by='zipcode') %>%
  filter(number > 550) %>%
  group_by(zipcode) %>%
  summarise(sd=sd(sqft_lot), mpbathroom=names(which.max(table(bathrooms))))

# Odp:

#    zipcode sd   najpopularniejsza liczba lazienek
#  
#    98038 63111. 2.5       
#    98052 10276. 2.5       
#    98103  1832. 1         
#    98115  2675. 1         
#    98117  2319. 1         


# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

df %>%
  filter(waterfront == 0) %>%
  mutate(area = case_when(sqft_living <= 2000 ~ '(0,2000]',
                          sqft_living <= 4000 ~ '(2000, 4000]',
                          T ~ '(4000, +Inf)')) %>%
  group_by(area) %>%
  summarise(avg_p = mean(price), p_median = median(price))
# Odp:

# area          mean     median

# (0,2000]      385084   359000
# (2000, 4000]  645419   595000
# (4000, +Inf) 1448119  1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df %>%
  mutate(sqmeters = sqft_living*0.09290304) %>%
  transmute(pricepersqft = (price/sqmeters)) %>%
  summarise(min(pricepersqft))

# Odp: 942.8 USD
