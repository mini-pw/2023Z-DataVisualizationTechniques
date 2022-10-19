library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

df %>% filter(bathrooms > median(df$bathrooms) & long>-122) %>%
  summarise(mean_price = mean(price))

# Odp: 625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>% group_by(yr_built) %>% summarise(n = n()) %>% arrange(-n)

# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?

woda = median(filter(df, waterfront == 1)$price)
lad = median(filter(df, waterfront == 0)$price)
(woda - lad)/lad * 100

# Odp: O około 211,11

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?

df %>% filter(floors == 1) %>% group_by(yr_built) %>%
  filter(price == min(price)) %>% ungroup() %>% summarise(mean(sqft_living))

# Odp: 1030

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

df %>% filter(bathrooms == 1 | bathrooms == 2) %>% select(grade) -> pom
pom = pom$grade
quantile(pom, probs = c(0.25,0.75))

# Odp: Tak jest róznica, Q1 = 6, a Q3 = 7.

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)

df %>% select(lat) -> wysokosci
wysokosc_max <- max(wysokosci)
wysokosc_min <- min(wysokosci)
srodek <- (wysokosc_max + wysokosc_min)/2

#północ
df %>% filter(lat > srodek) %>% select(price) -> pom
pom = pom$price
quantile(pom, probs = c(0.25,0.75)) -> pom
pom[2]-pom[1]

#południe
df %>% filter(lat < srodek) %>% select(price) -> pom
pom = pom$price
quantile(pom, probs = c(0.25,0.75)) -> pom
pom[2]-pom[1]

# Odp: Na północy odstęp jest równy 321000 USD, a na południu 122500 USD

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?

df  %>% filter(waterfront == 0 & sqft_living <= 1800) %>% group_by(bathrooms) -> pom
sort(table(pom$bathrooms))


# Odp: Najczęściej występuje 1 łazienka, najrzadziej 3.75

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek

df %>% group_by(zipcode) %>% summarise(n =n()) %>% filter(n > 550) -> poczta
poczta = poczta$zipcode
# 98038 98052 98103 98115 98117


# odchylenie standardowe

df %>% 
  filter(zipcode %in% poczta) %>% 
  group_by(zipcode) %>% 
  summarise(x = sd(sqft_lot))

#liczba łazienek

df %>% 
  filter(zipcode %in% poczta) %>% 
  group_by(zipcode, bathrooms) %>% 
  summarise(lazienki = n()) %>% 
  top_n(1, lazienki)


# Odp:Adres 98038 odchylenie - 63111.11, liczba łazienek - 2.5; 98052 odchylenie - 10276.19, liczba łazienek - 2.5;  98103 odchylenie - 1832.009, liczba łazienek - 1; 98115 odchylenie - 2675.302, liczba łazienek - 1; 98117 odchylenie - 2318.662, liczba łazienek - 1;

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

#(0,2000]
df %>% filter(waterfront == 0 & sqft_living <= 2000) %>% summarise(mean(price))
df %>% filter(waterfront == 0 & sqft_living <= 2000) %>% summarise(median(price))

#(2000,4000]
df %>% filter(waterfront == 0 & sqft_living > 2000 & sqft_living <= 4000) %>% summarise(mean(price))
df %>% filter(waterfront == 0 & sqft_living > 2000 & sqft_living <= 4000) %>% summarise(median(price))

#(4000,inf]
df %>% filter(waterfront == 0 & sqft_living > 4000 ) %>% summarise(mean(price))
df %>% filter(waterfront == 0 & sqft_living > 4000 ) %>% summarise(median(price))

# Odp: Dla (0,2000] średnia cena to 385084.3, a mediana  359000. Dla (2000,4000]  średnia cena to 645419, a mediana  595000. Dla (4000,inf]  średnia cena to 1448119, a mediana  1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df %>% mutate(metr_2 = price/(sqft_living/11)) %>% select(metr_2) %>% arrange(metr_2) %>% head(1)

# Odp: 963 dolarów 47 centów
