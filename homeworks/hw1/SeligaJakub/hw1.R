library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

df %>% 
  filter(bathrooms>(median(bathrooms)), long>-122) %>% 
  summarise(mean_price=mean(price))

# Odp: Ta średnia cena to $625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>% 
  group_by(yr_built) %>% 
  summarise(n()) %>% 
  top_n(1, `n()`)
  
# Odp: W 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?

water<-df %>% 
  filter(waterfront==T) %>% 
  summarise(med_water=median(price))
nonwater<-df %>% 
  filter(waterfront==F) %>% 
  summarise(med_water=median(price))

((water$med_water/nonwater$med_water)-1)*100

# Odp: O 211.(1)% większa

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?

df4<-df %>% 
  filter(floors==1) %>% 
  group_by(yr_built) %>% 
  filter(price == min(price)) %>% 
  summarise(mean_sqft_living=mean(sqft_living)) %>% 
  as.data.frame()

# Odp:
df4

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

onebath<-df %>% 
  filter(bathrooms==1) %>% 
  summarise(quant=quantile(grade,prob=c(.25,.75)))

twobaths<-df %>% 
  filter(bathrooms==2) %>% 
  summarise(quant=quantile(grade,prob=c(.25,.75)))

twobaths-onebath

# Odp: Jest różnica, wynosząca 1 punkt jakości zarówno w Q1 i Q3, na korzyść pomieszczenia z dwoma łazienkami

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)


eightmile<-mean(c(max(df$lat), min(df$lat)))

north<-df %>% 
  filter(lat>eightmile) %>% 
  summarise(iqr=IQR(price))

south<-df %>% 
  filter(lat<eightmile) %>% 
  summarise(iqr=IQR(price))

north
south
# Odp: Ten odstęp wynosi na północy $321000, na południu $122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?

df7<-as.data.frame(df %>% 
  filter(waterfront==F) %>% 
  mutate(sqft_per_floor=sqft_living/floors) %>% 
  filter(sqft_per_floor<=1800) %>% 
  group_by(bathrooms) %>% 
  summarise(n())) %>% 
  arrange(-`n()`)

head(df7,1)
tail(df7,1)

# Odp: Najczęściej 2.5, a najrzadziej (o ile nie zerowo) 4.75

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek

df %>% 
  group_by(zipcode) %>% 
  summarise(amount=n(), deviation_lot=sd(sqft_lot), bathrooms) %>% 
  filter(amount>550) %>% 
  group_by(zipcode, deviation_lot, bathrooms) %>% 
  summarise(amountB=n()) %>% 
  filter(amountB==max(amountB)) %>% 
  select(zipcode, deviation_lot, bathrooms) %>% 
  as.data.frame()

# Odp:
#  zipcode deviation_lot bathrooms
#1   98038     63111.112       2.5
#2   98052     10276.188       2.5
#3   98103      1832.009       1.0
#4   98115      2675.302       1.0
#5   98117      2318.662       1.0

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

df %>% 
  filter(waterfront==F) %>% 
  mutate(area = case_when(
    sqft_living <= 2000 ~ '(0, 2000]',
    sqft_living <= 4000 ~ '(2000,4000]',
    TRUE ~ '(4000, +Inf)')) %>% 
  group_by(area) %>% 
  summarise(mean_price=mean(price), median_price=median(price)) %>% 
  as.data.frame()

# Odp:
#          area mean_price median_price
#1    (0, 2000]   385084.3       359000
#2  (2000,4000]   645419.0       595000
#3 (4000, +Inf)  1448118.8      1262750


# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df %>% 
  mutate(price_per_sqft=price/sqft_living) %>% 
  select(price_per_sqft) %>% 
  filter(price_per_sqft==min(price_per_sqft)) %>% 
  mutate(price_per_sqm=price_per_sqft*10.7639)
  
# Odp: Najmniejsza cena za metr kwadratowy nieruchomości to $942.791
