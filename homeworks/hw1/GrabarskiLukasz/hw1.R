# TWD 18.10.2022
# Lukasz Grabarski


library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?
df %>% 
  filter(bathrooms > median(bathrooms), long > -122) %>% 
  summarise(mean(price))

# Odp: 625499.4


# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>% 
  group_by(yr_built) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)

# Odp: 2014


# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
df %>% 
  group_by(waterfront) %>% 
  summarise(median = median(price))  
1400000 / 450000 *100

# Odp: 211%


# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?

df %>% 
  filter(floors == 1) %>% 
  select(price, sqft_above, yr_built) %>% 
  group_by(yr_built) %>% 
  top_n(-10, price) %>% 
  summarise( mean_sqft = mean(sqft_above)) -> zad4
  
# Odp: pelny wynik znajduje sie w zmiennej zad4, ponizej kilka pierwszych pozycji:
# yr_built mean_sqft
# <int>     <dbl>
#   1     1900      892.
# 2     1901     1000 
# 3     1902     1150 
# 4     1903      923 
# 5     1904      880 
# 6     1905      916 
# 7     1906      908 
# 8     1907      866 
# 9     1908      923 
# 10     1909      954 


# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?
df %>% 
  select(bathrooms, grade) %>% 
  filter(bathrooms == 1 | bathrooms == 2) %>% 
  group_by(bathrooms) %>% 
  summarise(kwartyl = quantile(grade)) 
  
# Odp: Q1 i Q3 są o 1 wyzysze dla mieszkan z dwoma lazienkami 


# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)
middle <- summarise(df, min(lat), max(lat))
middle <- (middle$`min(lat)` + middle$`max(lat)`)/2

df %>% 
  select(price, lat) %>% 
  mutate(lat_section = if_else(lat > middle, "north", "south")) %>% 
  group_by(lat_section) %>% 
  summarise(IQR = IQR(price))
  
# Odp:1 north       321000, 2 south       122500


# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
df %>% 
  filter(waterfront == 0, sqft_above <= 1800) %>%
  select(bathrooms) %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(Freq) -> baths_freq
head(baths_freq, 1)
tail(baths_freq, 1)

# Odp: najczescie: 1 - 3767 razy, najrzadziej 4.5 -  1 raz


# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
df %>%
  group_by(zipcode) %>% 
  summarise(n = n()) %>% 
  filter(n > 550) %>% 
  select(zipcode) %>% 
  unlist() -> zips
  
for(zip in zips){
  print(zip)
  df %>% 
    filter(zipcode == zip) %>% 
    summarise(sd = sd(sqft_lot)) -> res1
  print(res1)
  
  df %>% 
    filter(zipcode == zip) %>% 
    select(bathrooms) %>% 
    table() %>% 
    as.data.frame() %>% 
    arrange(-Freq) %>% 
    head(1) -> res2
  print(res2)
}
  
  # Odp:
# [1] 98038
# sd
# 1 63111.11
# bathrooms Freq
# 1       2.5  334
# [1] 98052
# sd
# 1 10276.19
# bathrooms Freq
# 1       2.5  216
# [1] 98103
# sd
# 1 1832.009
# bathrooms Freq
# 1         1  167
# [1] 98115
# sd
# 1 2675.302
# bathrooms Freq
# 1         1  162
# [1] 98117
# sd
# 1 2318.662
# bathrooms Freq
# 1         1  178


# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.
df %>% 
  filter(waterfront == 0) %>% 
  mutate(interval = case_when(sqft_living <= 2000 ~ "0 - 2000",
                              sqft_living <= 4000 ~ "2000 - 4000",
                              TRUE ~ "4000+")) %>% 
  group_by(interval) %>% 
  summarise(mean = mean(price), median = median(price))

# Odp:
# interval        mean  median
# <chr>          <dbl>   <dbl>
#   1 0 - 2000     385084.  359000
# 2 2000 - 4000  645419.  595000
# 3 4000+       1448119. 1262750


# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
df %>% 
  mutate(sqm = sqft_living * 0.09290304) %>% 
  transmute(price_per_sqm = price / sqm) %>% 
  min()

# Odp: 942.7919 USD / m^2
