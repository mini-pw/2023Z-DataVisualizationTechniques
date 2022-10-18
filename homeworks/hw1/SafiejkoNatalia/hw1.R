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
    

# Odp: 625499.4

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

# Odp: 211.11%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
  
    df %>% 
      filter(floors==1) %>% 
      select(yr_built, price, sqft_living) -> h1
    h1
    
    df %>% 
      filter(floors==1) %>% 
      group_by(yr_built) %>% 
      select(yr_built, price, sqft_living) %>% 
      summarise(price = min(price)) -> h2
    
    h2
    
    inner_join(h2,h1) %>% 
      summarise(mean(sqft_living))
      
  
# Odp: 1030

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

  df %>% 
    filter(bathrooms==1 | bathrooms==2) %>% 
    select(grade) ->x
  
  x=x$grade
  
  quantile(x, prob=c(0.25, 0.75))
  
# Odp: Tak, różnią się q1=6 a q3=7

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)
  df %>% 
    arrange(lat) %>% 
    select(lat) %>% 
    head(1) ->val1
   
  df %>% 
    arrange(-lat) %>% 
    select(lat) %>% 
    head(1) -> val2

srodek <- (val1+val2)/2
srodek <- srodek[1,1]
srodek

#polnoc

df %>% 
  filter(lat>=srodek) %>%   
  select(price) ->x

x=x$price
a <- quantile(x, prob=c(0.25, 0.75))
a
a[2]-a[1]

#poludnie

df %>% 
  filter(lat<=srodek) %>%   
  select(price) ->x
x=x$price
a <- quantile(x, prob=c(0.25, 0.75))
a
a[2]-a[1]

# Odp: na północy: 321000, na południu: 122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
df %>% 
  filter(waterfront==0) %>% 
  mutate(sqft_onefloor= sqft_living/floors) %>% 
  filter(sqft_onefloor<=1800)  %>% 
  group_by(bathrooms) %>% 
  summarise(n=n()) %>% 
  arrange(n) ->dfh

head(dfh, 1)

dfh %>% 
  arrange(-n) %>% 
  head(1)

# Odp: najrzadziej: 4.75, najczęściej: 2.5

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
df %>% 
  group_by(zipcode) %>% 
  summarise(n=n()) %>% 
  filter(n>550) %>% 
  select(zipcode) -> kody

kody <- kody$zipcode
kody

df %>% 
  filter(zipcode %in% (kody)) %>% 
  group_by(zipcode, bathrooms) %>% 
  summarise(n=n()) ->r1
  r1
  
  df %>% 
    filter(zipcode %in% (kody)) %>% 
    group_by(zipcode, bathrooms) %>% 
    summarise(n=n()) %>% 
    summarise(n= max(n)) ->r2
  r2
  
inner_join(r1,r2)

df %>% 
  filter(zipcode %in% (kody)) %>% 
  group_by(zipcode) %>% 
  select(zipcode, sqft_lot) %>% 
  summarise(sd(sqft_lot))
  




# Odp:Odchylenie standardowe dla każdego zipkodu - 98038: 63111; 98052:10276;  98103:1832;  98115:2675;  98117: 2319
# Najpopularniejsza liczba łazienek: 98038: 2.5 ; 98052: 2.5 ; 98103: 1 ;  98115:1 ; 98117:1

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.
df %>% 
  filter(waterfront==0) ->df1
#(0,2000]
df1 %>% 
  filter(sqft_living>0 & sqft_living<=2000) %>% 
  summarise(mean=mean(price), median= median(price))

#(2000,4000]
df1 %>% 
  filter(sqft_living>2000 & sqft_living<=4000) %>% 
  summarise(mean=mean(price), median= median(price))

#(4000, +Inf)
df1 %>% 
  filter(sqft_living>4000 ) %>% 
  summarise(mean=mean(price), median= median(price))


# Odp:
#(0,2000]: srednia: 385084.3, mediana: 359000
#(2000,4000]: srednia: 645419, mediana: 595000
#(4000, +Inf): srednia: 1448119, mediana: 1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
df %>% 
  transmute(price_m2= price/(sqft_living*0.09290304)) %>% 
  arrange(price_m2) %>% 
  head(1)

# Odp: 942.7919

