library('dplyr')

#setwd("C:/Users/Dell/Documents/studia/TWD")
df <- read.csv("house_data.csv")


colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

df %>%
  filter(bathrooms > median(bathrooms) & long > -122) %>% 
  summarise(srednia = mean(price))


# Komentarz: Liczba łazienek niecałkowita?

# Odp: 625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?


df %>% 
  group_by(yr_built) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)

# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?

median_water_front <- df %>% 
  filter(waterfront == 1) %>% 
  summarise(median_water_front = median(price))

median_water_front_no <- df %>% 
  filter(waterfront == 0) %>% 
  summarise(median_water_front_no = median(price))

median_water_front[1,1]/median_water_front_no[1,1]*100-100

# Odp: 211%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?

df %>%
  filter(floors == 1) %>% 
  group_by(yr_built) %>% 
  mutate(min_values = min(price)) %>% 
  filter(min_values==price,) %>%
  ungroup() %>% 
  summarise(mean=mean(sqft_living))
  

# Odp: 1030

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

df %>% 
  filter(bathrooms == 1) %>% 
  summarise(Q1 = quantile(grade, probs = .25), 
            Q3 = quantile(grade, probs = .75))
df %>% 
  filter(bathrooms == 2) %>% 
  summarise(Q1 = quantile(grade, probs = .25), 
            Q3 = quantile(grade, probs = .75))

# Odp:  1 łazienka: Q1=6, Q3=7
#       2 łazienki: Q1=7, Q3=8

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)

midvalue <- (min(df$lat) + max(df$lat)) / 2

df %>%  
  filter(lat > midvalue) %>% 
  summarise(odstep = IQR(price))

df %>%  
  filter(lat < midvalue) %>% 
  summarise(odstep = IQR(price))

# Odp: północnych: 321000, południowych: 122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?

bathroom_count <- df %>% 
  filter(waterfront == 0 & sqft_living/floors <= 1800) %>% 
  group_by(bathrooms) %>% 
  summarise(n = n()) %>% 
  arrange(n)
head(bathroom_count,1)
tail(bathroom_count,1)

# Odp: Najczęściej: 2.5, Najrzadziej: 4.75

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek

zipcodes <- df %>% 
  group_by(zipcode) %>% 
  summarise(n = n()) %>% 
  filter(n > 550) %>% 
  select(zipcode)

zipcodes <- as.vector(zipcodes$zipcode)
length(zipcodes)

for (i in zipcodes) {
  print(i)
  sd <- df %>% 
    filter(zipcode == i) %>% 
    summarise(sd = sd(sqft_lot))
  print(sd)
  df %>% 
    filter(zipcode == i) %>%
    group_by(bathrooms) %>% 
    summarise(n = n()) %>% 
    arrange(n) %>% 
    tail(1) %>% 
    print()
}

# Odp:  kod pocztowy: 98038, odchylenie standardowe: 63111.11, najpopularniejsza liczba łazienek: 2.5
#       kod pocztowy: 98052, odchylenie standardowe: 10276.19, najpopularniejsza liczba łazienek: 2.5
#       kod pocztowy: 98103, odchylenie standardowe: 1832.009, najpopularniejsza liczba łazienek: 1
#       kod pocztowy: 98115, odchylenie standardowe: 2675.302, najpopularniejsza liczba łazienek: 1
#       kod pocztowy: 98117, odchylenie standardowe: 2318.662, najpopularniejsza liczba łazienek: 1

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

df %>% 
  filter(sqft_living <= 2000 & waterfront == 0) %>% 
  summarise(mean = mean(price), median = median(price))
df %>% 
  filter(sqft_living <= 4000 & sqft_living > 2000 & waterfront == 0) %>% 
  summarise(mean = mean(price), median = median(price))
df %>% 
  filter(sqft_living > 4000 & waterfront == 0) %>% 
  summarise(mean = mean(price), median = median(price))

# Odp:  (0, 2000]: średnia > mediana
#       (2000,4000]: średnia > mediana
#       (4000, +Inf): średnia > mediana

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df %>% 
  mutate(cena_za_metr = price/(sqft_living*0.092903411613275)) %>% 
  arrange(cena_za_metr) %>% 
  select(cena_za_metr) %>% 
  head(1)

# Odp:  942.7881