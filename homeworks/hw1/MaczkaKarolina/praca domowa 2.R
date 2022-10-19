library(dplyr)
df<-read.csv('https://raw.githubusercontent.com/MI2-Education/2023Z-DataVisualizationTechniques/main/homeworks/hw1/house_data.csv')
head(df)
colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

df %>% 
  filter(bathrooms > median(bathrooms, na.rm = TRUE), long<(-122)) %>% 
  summarise(mean = mean(price, na.rm = TRUE))

# Odp:697993.6

# 2. W którym roku zbudowano najwięcej nieruchomości?
df %>% 
  group_by(yr_built) %>% 
  count(yr_built,name="Number") %>% 
  arrange(desc(Number)) %>% 
  head(1)

# Odp:2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
nie_nad_wodą<-df %>% 
  filter(waterfront==0) %>% 
  summarise(median = median(price, na.rm = TRUE))
nad_wodą<-df %>% 
  filter(waterfront!=0) %>% 
  summarise(median = median(price, na.rm = TRUE))
nad_wodą/nie_nad_wodą*100

# Odp: 211%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
df %>% 
  filter(floors==1) %>% 
  group_by(yr_built) %>% 
  filter(price==min(price)) %>% 
  ungroup() %>% 
  summarise(mean = mean(sqft_living,na.rm = TRUE))

# Odp: 1030

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

df %>% 
  filter(bathrooms %in% c(1,2)) %>% 
  group_by(bathrooms) %>% 
  summarise(quantile1 = quantile(grade, probs=0.25), quantile3 = quantile(grade, probs = 0.75))

# Odp: Tak, wartość pierwszego i trzeciego kwartyla dla mieszkań z jedną łązienką wynoszą odpowiednio 6 i 7, a z dwoma 7 i 8.

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)

pomiedzy <- (max(df$lat)+min(df$lat))/2

df %>% 
  select(price, lat) %>% 
  mutate(polozenie = ifelse(df$lat>pomiedzy, 'polnoc', 'poludnie')) %>% 
  group_by(polozenie) %>% 
  summarise(quantile1 = quantile(price, probs=0.25), quantile3 = quantile(price, probs = 0.75)) %>% 
  mutate(roznica = quantile3 - quantile1)

# Odp: Różnica wynosi 321000 dla północy i 122500 dla południa

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
zadanko7 <- df %>% 
  filter(waterfront==0,sqft_living/floors<=1800) %>% 
  group_by(bathrooms) %>% 
  count(bathrooms) %>% 
  arrange(desc(n))
zadanko7$bathrooms

# Odp: najczęśćiej - 2.5, najrzadziej - 4.75

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
above550 <- df %>% 
  group_by(zipcode) %>% 
  count(zipcode) %>% 
  filter(n>550)
  
#łazienki
df %>% 
  filter(zipcode %in% above550$zipcode) %>% 
  group_by(zipcode) %>% 
  count(bathrooms) %>%
  group_by(zipcode) %>%
  mutate(maks = max(n)) %>% 
  filter(n==maks)

#odchylenie standardowe
df %>% 
  filter(zipcode %in% above550$zipcode) %>% 
  group_by(zipcode) %>% 
  summarise(odchylenie_stand = sd(sqft_lot))
  

# Odp: zipcode - bathrooms - sd
# 98038 - 2.5 - 6311  
# 98052 - 2.5 - 10276  
# 98103 - 1 - 1832 
# 98115 - 1 - 2675  
# 98117 - 1 - 2319

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.
df %>% 
  filter(waterfront==0) %>% 
  filter(sqft_living>0 & sqft_living<=2000) %>% 
  summarise(srednia=mean(price), mediana = median(price) )
  
df %>% 
  filter(waterfront==0) %>% 
  filter(sqft_living>2000 & sqft_living<=4000) %>% 
  summarise(srednia=mean(price), mediana = median(price) )

df %>%
  filter(waterfront==0) %>% 
  filter(sqft_living>4000) %>% 
  summarise(srednia=mean(price), mediana = median(price) )

# Odp: przedział średnia mediana
# (0, 2000] 385084.3  359000
# (2000,4000] 645419  595000
# (4000, +Inf) 1448119 1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
df %>% 
  summarise(cena=price/(sqft_living*0.093)) %>% 
  arrange(cena) %>% 
  head(1)

# Odp: 941.809