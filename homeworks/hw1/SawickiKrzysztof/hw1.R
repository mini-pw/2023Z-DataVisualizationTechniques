library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

df %>% 
  filter(bathrooms>median(bathrooms, na.rm = T), long>-122) %>% 
  select(price) %>% 
  summarise(cena = mean(price))


# Odp:625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>% 
  group_by(yr_built) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>% 
  head(1)

# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?

df %>% 
  filter(waterfront==1) %>%
  summarise(mediana_nad_woda = median(price)) -> mediana_nad_woda

df %>% 
  filter(waterfront==0) %>% 
  summarise(mediana_nie_nad_woda = median(price)) -> mediana_nie_nad_woda

wynik <- 100*(mediana_nad_woda[1] - mediana_nie_nad_woda)/mediana_nie_nad_woda
wynik

# Odp:  211.1111%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?

df %>% 
  filter(floors==1) %>% 
  group_by(yr_built) %>% 
  summarise(price = min(price)) %>% 
  inner_join(df[df$floors == 1,], by= c('yr_built','price')) %>% 
  summarise(mean(sqft_living))


# Odp: 1030 stóp kwadratowych

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

  
  df %>% 
    filter(bathrooms==1) %>% 
    summarise(quantile(grade)) -> kwartyl
  
  kwartyl[4,1] - kwartyl[2,1]
  
  df %>% 
    filter(bathrooms==2) %>% 
    summarise(quantile(grade)) -> kwartyl2
  
  kwartyl2[4,1] - kwartyl2[2,1]


  # Odp: Tak, jest różnica. Dla mieszkań z jedną łazienką q3 = 7 a q1 = 6 zatem różnica wynosi 1
        #Dla mieszkań z dwoma łazienkami q3 = 8 a q1 = 7 zatem różnia również wynosi 1

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)

  df %>% 
    arrange(lat) %>% select(lat) ->df_lat
  srodek <- (df_lat[nrow(df_lat),1]-df_lat[1,1])/2 + df_lat[1,1]
  
  df %>% 
    mutate(where = ifelse(lat >= srodek,"Polnoc","Poludnie")) %>% 
    group_by(where) %>% 
    summarise(iqr = IQR(price))
  


# Odp: Odstęp na północy wynosi 321000, natomiast na południu 122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?

  df %>% 
    filter(waterfront==0) %>% 
    mutate(sqft_onefloor= sqft_living/floors) %>% 
    filter(sqft_onefloor<=1800)  %>% 
    group_by(bathrooms) %>% 
    summarise(n=n()) %>% 
    arrange(n) %>% head(1)
  
  
  dfh %>% 
    arrange(-n) %>% 
    head(1)

# Odp: Najczęściej ilość łazienek wynosi 2.5, a najrzadziej 4.75

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
  
  df %>% 
    group_by(zipcode) %>% 
    summarise(n = n()) %>% 
    filter(n > 550) %>%  pull(zipcode) -> kody  

  
  df %>% 
    filter(zipcode %in% (kody)) %>% 
    group_by(zipcode, bathrooms) %>% 
    summarise(n=n()) -> łazienki
  
  df %>% 
    filter(zipcode %in% (kody)) %>% 
    group_by(zipcode, bathrooms) %>% 
    summarise(n=n()) %>%  
    summarise(n = max(n))  %>% 
    left_join(łazienki, by= c('zipcode','n')) %>% head()


  
  df %>% 
    filter(zipcode %in% (kody)) %>% group_by(zipcode) %>% summarise(odchylenie = sd(sqft_lot)) %>% head()
  
# Odp: 
  #Dla kodu 98038: liczba łazienek - 2.5, odchylenie standardowe - 63111
  #Dla kodu 98052: liczba łazienek - 2.5, odchylenie standardowe - 10276
  #Dla kodu 98103: liczba łazienek - 1, odchylenie standardowe - 1832
  #Dla kodu 98115: liczba łazienek - 1, odchylenie standardowe - 2675
  #Dla kodu 98117: liczba łazienek - 1, odchylenie standardowe - 2319
  

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

  df %>% 
    filter(waterfront == 0) %>% mutate(przedzial =case_when(
      sqft_living <= 2000 ~ "(0, 2000]", 
      sqft_living <= 4000 & sqft_living > 2000 ~ "(2000,4000]",
      TRUE ~ "(4000, +Inf)")) %>% select(price,przedzial) %>% 
      group_by(przedzial) %>%
      summarise(srednia = mean(price), mediana = median(price)) %>% 
    head()
    

  

# Odp: 
  #przedzial    srednia   mediana
  #(0, 2000]    385084    359000
  #(2000,4000]  645419    595000
  #(4000, +Inf) 1448119   1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
  
  df %>% 
    select(id,price,sqft_living) %>% mutate(m_living = sqft_living/10.764, cena_za_m = price/m_living) %>%  arrange(cena_za_m) %>%head(1)


# Odp: Najmniejsza cena za metr kwadratowy wynosi 942.7998 i dotyczy mieszkania 2891000610

