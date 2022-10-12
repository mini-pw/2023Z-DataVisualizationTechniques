library(dplyr)

df <- read.csv("house_data.csv", stringsAsFactors = FALSE)

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

df %>%
  filter(long > -122 & bathrooms>median(bathrooms)) %>%
  summarise(median=median(price))

# Odp: 563500

# 2. W którym roku zbudowano najwięcej nieruchomości?
df %>% 
  group_by(yr_built) %>%
  summarise(n = n()) %>%
  arrange(-n) %>% head(1)

# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
df %>% 
  group_by(waterfront) %>% 
  summarise(median = median(price)) %>% 
  summarise(ratio = median/median[waterfront==0]) -> water_price
(water_price[2]-1)*100

# Odp: o 211%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
df %>%
  filter(floors==1) %>%
  group_by(yr_built) %>% 
  arrange(price) %>% 
  slice_head(n=1) %>% 
  ungroup() %>% 
  summarise(mean = mean(sqft_living))

# Odp: 1023 sqft

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?
df %>% 
  filter(bathrooms==1 | bathrooms==2) %>% 
  group_by(bathrooms) %>% 
  arrange(grade) %>% 
  summarise(Q1 = last(head(grade, prop = 0.25)), Q3 = first(tail(grade, prop = 0.25)))

# Odp:  zarówno Q1 i Q3 różnią się o jeden

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)
df %>%
  mutate(s_n = ifelse(lat < (min(lat)+max(lat))/2, "south", "north")) %>% 
  group_by(s_n) %>% 
  arrange(price) %>% 
  summarise(Q1 = last(head(price, prop = 0.25)), Q3 = first(tail(price, prop = 0.25))) %>% 
  mutate(dif = Q3-Q1)

# Odp: północ: $5217000, południe: $1485000

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
df %>% 
  filter(waterfront == 1 & sqft_living <= 1800) %>% 
  group_by(bathrooms) %>% 
  summarise(n=n()) %>% 
  filter(n==max(n) | n==min(n))

# Odp: Najczęściej występuje 1 łazienka, najrzadziej 1.5, 2, 2.25, 3.25

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
mlv <- function(codes){which.max(tabulate(codes))}    # moda
df %>% 
  group_by(zipcode) %>% 
  summarise(n=n(), sd = sd(sqft_lot), bath = mlv(bathrooms)) %>% 
  filter(n>550)

# Odp:

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.
df %>% 
  filter(waterfront == 0) %>% 
  mutate(size = case_when(sqft_living <= 2000 ~"small",
                          sqft_living <= 4000~ "medium",
                          TRUE ~ "large")) %>% 
  group_by(size) %>% 
  summarise(mean=mean(price), median=median(price))

# Odp:
#          srednia  mediana
# duze     1448119  1262750
# srednie   645419   595000
# male      385084   359000

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
ft_to_m = 0.3048
df %>%
  mutate(price_per_sqrm = price/(sqft_living*ft_to_m^2)) %>% 
  arrange(price_per_sqrm) %>% 
  head(1)

# Odp: $942.79

