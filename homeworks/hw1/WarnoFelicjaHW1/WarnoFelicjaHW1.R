library(dplyr)

df <- read.csv(url("https://raw.githubusercontent.com/MI2-Education/2023Z-DataVisualizationTechniques/main/homeworks/hw1/house_data.csv"))

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?
df %>%
  filter(long > -122, bathrooms > median(bathrooms)) %>% 
  summarise(mean_price = mean(price))

# Odp: 625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?

df%>%
  group_by(yr_built) %>% 
  summarise(count = n()) %>%
  top_n(1, count) %>% 
  select(yr_built)
# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
nw <- df %>% 
  filter(waterfront == 1) %>% 
  summarise(mean_price = mean(price))
bw <- df %>% 
  filter(waterfront == 0) %>% 
  summarise(mean_price = mean(price))
odp3 = (nw*100)/bw - 100
  
# Odp: 213

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
df %>% 
  group_by(yr_built) %>% 
  filter(price == min(price), floors == 1) %>% 
  ungroup() %>% 
  summarize(mean = mean(sqft_living)) %>% 
  as.data.frame()

# Odp:955.6098

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

b1 <- df %>% 
  filter(bathrooms == 1) %>% 
  select(grade) %>% 
  summary()

b2 <- df %>% 
  filter(bathrooms == 2) %>% 
  select(grade) %>% 
  summary()
  

# Odp: Tak wartość wśród nieruchomości z jedną łazienką to 6 w pierwszym kwartylu i 7 w trzecim kwartylu, a z dwiema łazienkami to 7 w pierwszym kwartylu i 8 w trzecim kwartylu.

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)
punkt = (min(df$lat) + max(df$lat)) / 2
df %>%
  mutate(PP = ifelse(lat > punkt, "Polnoc", "Poludnie")) %>%
  group_by(PP) %>%
  summarise(iqr = IQR(price))

# Odp: Odstęp międzykwartylowy dla północy to 321000, a dla południa to 122500.

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
max <- df %>% 
  filter(waterfront == 0, sqft_living/floors < 1800) %>% 
  group_by(bathrooms) %>% 
  summarize(count = n()) %>% 
  top_n(1, count)

min <- df %>% 
  filter(waterfront == 0, sqft_living/floors < 1800) %>% 
  group_by(bathrooms) %>% 
  summarize(count = n()) %>% 
  top_n(-1, count)

# Odp: Najczęściej występuje liczba 2.5, a najrzadziej 4.75

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek

kody <- df %>%
  group_by(zipcode) %>% 
  summarize(count = n()) %>%
  filter(count > 550) %>% 
  merge(df)

a <- kody %>%
  group_by(zipcode) %>% 
  summarise(SD = sd(sqft_lot))

b <- kody %>%
  group_by(bathrooms, zipcode) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(zipcode) %>% 
  filter(count == max(count))

odp8 <- merge(a, b)
  
  
# Odp:

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

x <- df %>% 
  filter(waterfront == 0)
#(0, 2000]
df1 <- filter(x, sqft_living > 0 & sqft_living <= 2000) %>%
  summarise(mean = mean(price), median = median(price))
#(2000,4000]
df2 <- filter(x, sqft_living > 2000 & sqft_living <= 4000) %>%
  summarise(mean = mean(price), median = median(price))
#(4000, +inf)
df3 <- filter(x, sqft_living > 4000) %>%
  summarise(mean = mean(price), median = median(price))
# Odp:  
#(0, 2000]: mean = 385084.3, median = 359000
#(2000, 4000]: mean = 645419, median = 595000
#(4000, +inf): mean = 1448119, median = 1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df %>% 
  mutate(cena_m2 = price/(0.092903*sqft_living)) %>%
  top_n(-1) %>% 
  select(cena_m2)

# Odp: 942.7923
