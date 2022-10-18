library(dplyr)
setwd("C:/Users/Ag/Desktop/Studia/sem3/TWD")
df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie
View(df)
str(df)

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

temp<-median(df$bathrooms)

df %>% 
  filter(long>-122,bathrooms>temp) %>%
  summarise(mean_price = mean(price, na.rm = TRUE))

# Odp:  625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>% 
  group_by(yr_built) %>% 
  summarise(n=n()) %>% 
  top_n(1,n)

# Odp:  2014r - 559

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?

df %>% 
  group_by(waterfront) %>% 
  summarise(median_price = median(price)) %>% 
  mutate(ratio = (median_price-median_price[waterfront == 0])/median_price[waterfront == 0]*100)

# Odp:  211%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
df %>% 
  filter(floors == 1) %>% 
  group_by(yr_built) %>% 
  top_n(1,-price) -> temp
  mean(temp$sqft_living)

# Odp:  1030.422

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?
df %>% 
  filter(bathrooms == 1 | bathrooms == 2) %>% 
  select(bathrooms,grade) %>% 
  group_by(bathrooms) %>% 
  summarize(qw1= quantile(grade, probs = 0.25), qw3 = quantile(grade, probs = 0.75))

# Odp:  jest różnica wartości 1 zarówno dla 1(6 i 7 grade) i 3(7 i 8 grade) kwantyla pomiędzy nieruchomościami z 1 i 2 łazienkami na korzyści większej ilości łazienek

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)

df %>% 
  select(lat,price) %>% 
  mutate(north =ifelse( lat < min(df$lat)+(max(df$lat)-min(df$lat))/2,0,1)) %>% 
  group_by(north) %>% 
  summarize(qw1= quantile(price, probs = 0.25), qw3 = quantile(price, probs = 0.75)) %>% 
  mutate(difference = qw3-qw1)

# Odp:  różnica między 1 i 3 kwartylem ceny mieszkań położonych na północy wynosi 321000, a na południu 122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?

df %>% 
  filter(waterfront == 0 & sqft_living/floors <= 1800) %>% 
  group_by(bathrooms) %>% 
  summarise(n=n()) %>% 
  filter(n==max(n) | n == min(n))
  
# Odp: najczęściej: 2.5 łazienek, najrzadziej: 4.75

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
df %>% 
  group_by(zipcode) %>% 
  summarise(n=n(), sd = sd(sqft_lot)) %>% 
  filter(n>550)->temp
temp

df %>% 
  filter(zipcode %in% temp$zipcode) %>% 
  group_by(zipcode, bathrooms) %>% 
  summarise(n = n()) %>% 
  top_n(1,n)

# Odp: zipcodes: 98038 98052 98103 98115 98117, sd odpowiednio: 63111.112 10276.188  1832.009  2675.302  2318.662
# najpopularniejsza liczba łazienek: 2.5 2.5 1.0 1.0 1.0


# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

df %>% 
  filter(waterfront == 0) %>% 
  mutate(intervals = ifelse(sqft_living<= 2000,1,ifelse(sqft_living<=4000,2,3))) %>% 
  group_by(intervals) %>% 
  summarise(mean = mean(price), median = median(price))

# Odp: dla przediału pierwszego - (0, 2000] : mediana ceny (35900) jest niższa od średniej(385084) 
#      dla przediału drugiego - (2000,4000] : mediana ceny (59500) jest niższa od średniej(645419) 
#      dla przediału trzeciego - (4000, +Inf) : mediana ceny (1262750) jest niższa od średniej(1448119) 

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df %>% 
  mutate(price_per_sqm = price/sqft_living*10.76391) %>% 
  select(price_per_sqm) %>% 
  top_n(1, -price_per_sqm)

# Odp: 942.7919

