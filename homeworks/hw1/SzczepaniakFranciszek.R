library(dplyr)

df <- read.csv("https://raw.githubusercontent.com/MI2-Education/2023Z-DataVisualizationTechniques/main/homeworks/hw1/house_data.csv")
View(df)
colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?
df %>% 
  select(bathrooms,long,price) %>%
  filter(median(bathrooms)< bathrooms,long > -122) %>% 
  summarise(mean_price = (mean(price))) %>% 
  View()

# Odp: 	625499.4


# 2. W którym roku zbudowano najwięcej nieruchomości?
df %>% 
  group_by(yr_built) %>% 
  summarise(how_many = n()) %>% 
  arrange(-how_many) %>% 
  head(1)


# Odp: 2014


# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
water <- df %>% select(waterfront,price) %>% filter(waterfront == 1) %>% summarise(median = median(price))
water
land <- df %>% select(waterfront,price) %>% filter(waterfront == 0) %>%  summarise(median = median(price))
land
wynik <- ((1400000/450000) * 100 ) - 100
wynik




# Odp: O 211.1111%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
df %>% filter(floors == 1.0) %>% group_by(yr_built) %>%  filter(price == min(price)) -> pom
mean(pom$sqft_living)

# Odp: 1030

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?
df %>% 
  filter(bathrooms == 1 | bathrooms == 2) %>% 
  group_by(bathrooms) %>% 
  summarise(pierwszy_kwartyl = quantile(grade,0.25), trzeci_kwartyl = quantile(grade,0.75))

# Odp:W obu przyadkach różnica wynosi 1.

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)
srodek <- (min(df$lat) + max(df$lat))/2
 df %>% filter(lat > 47.46675) -> north
 df %>% filter(lat < 47.46675) -> south
north %>% summarise(q = quantile(price))
south %>% summarise(q = quantile(price))

# Odp:Na polnocy: 390000 - 78000 = 321000
# Na poludniu: 367500 - 245000 = 122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
df %>% 
  filter(waterfront==0,sqft_living/floors < 1800) %>% 
  group_by(bathrooms) %>% 
  summarise(n=n()) -> pom

pom %>% 
  arrange(n) %>% 
  head(1)
pom %>% 
  arrange(-n) %>% 
  head(1)

# Odp: najczesciej 2.5, najrzadziej 4.75

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
df %>% 
  group_by(zipcode) %>% 
  summarise(n=n()) %>% 
  filter(n>550) %>% 
  select(zipcode) -> pom

df %>% 
  filter(zipcode %in% pom$zipcode) %>%
  group_by(zipcode,bathrooms) %>% 
  summarise(n=n()) %>%
  top_n(1,n)
 
df %>% 
  filter(zipcode %in% pom$zipcode) %>%
  group_by(zipcode) %>% 
  summarise(x = sd(sqft_lot))




# Odp:Dla 98038, 98052, 98103, 98115, 98117 kolejno 
# łazienki: 2.5 , 2.5 , 1 , 1 , 1
# odchylenie 63111  ,  10276  ,  1832  ,  2675  ,  2319

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.
df %>% filter(waterfront == 0) %>% filter(sqft_living <= 2000) %>% filter(sqft_living>0) -> dat1
df %>% filter(waterfront == 0) %>% filter(sqft_living <= 4000) %>% filter(sqft_living>2000)  -> dat2
df %>% filter(waterfront == 0) %>%  filter(sqft_living>4000) -> dat3
median(dat1$price)
median(dat2$price)
median(dat3$price)
mean(dat1$price)
mean(dat2$price)
mean(dat3$price)



# Odp: Mediany odpowiednio: 359000, 595000, 1267250    
#     Średnie odpowiednio: 385084.3, 645419, 1448119

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
df %>% 
  mutate(price_for_sqm = price/(sqft_living*0.0929)) %>% arrange(price_for_sqm) %>%select(price_for_sqm) %>%  head(1)

# Odp: 942.822 USD/m^2