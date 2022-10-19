library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartosci NA w zadnej kolumnie

# 1. Jaka jest srednia cena nieruchomosci z liczba lazienek powyzej mediany i polozonych na wschód od poludnika 122W?

df %>% 
  summarise(median(bathrooms)) #Mediana liczby lazienek to 2.25, wiec szukamy takich, co maja ponad 2.25 lazienki
df %>% 
  select(price,bathrooms,long) %>% 
  filter(bathrooms>2.25,long>-122) %>% 
  summarise(mean(price))
# Odp: 625499.4

# 2. W którym roku zbudowano najwiecej nieruchomosci?
df %>% 
  select(yr_built) %>% 
  group_by(yr_built) %>% 
  summarise(count = n()) %>% 
  top_n(1, count)

# Odp: 2014

# 3. O ile procent wieksza jest mediana ceny budynków polozonych nad woda w porównaniu z tymi polozonymi nie nad woda?
df %>% 
  select(price,waterfront) %>% 
  filter(waterfront==1) %>% 
  summarise(median(price)) # Mediana to 1400000
df %>% 
  select(price,waterfront) %>% 
  filter(waterfront==0) %>% 
  summarise(median(price)) #Mediana to 450000

(1400000/450000-1)*100

# Odp:211.1111%

# 4. Jaka jest srednia powierzchnia wnetrza mieszkania dla najtanszych nieruchomosci posiadajacych 1 pietro (tylko parter) wybudowanych w kazdym roku?
df %>% 
  select(price,floors,sqft_living,yr_built) %>% 
  filter(floors == 1) %>% 
  group_by(yr_built) %>% 
  summarise(min_price = min(price),price,sqft_living,yr_built) %>% 
  filter(price==min_price) %>% 
  select(sqft_living) %>% 
  summarise(sqft_living=mean(sqft_living)) %>%  
  summarise(sqft_living=mean(sqft_living)) 
  
# Odp: 1026 sqft

# 5. Czy jest róznica w wartosci pierwszego i trzeciego kwartyla jakosci wykonczenia pomieszczen pomiedzy nieruchomosciami z jedna i dwoma lazienkami? Jesli tak, to jak rózni sie Q1, a jak Q3 dla tych typów nieruchomosci?
df %>% 
  select(grade,waterfront) %>% 
  filter(waterfront==1) %>% 
  summarise(quantile(grade)) # Pierwszy kwartyl to 8, a trzeci 10
df %>% 
  select(grade,waterfront) %>% 
  filter(waterfront==0) %>% 
  summarise(quantile(grade)) #Pierwszy kwartyl to 7, a trzeci 8

# Odp:Jest roznica, kwantyle podane wyzej

# 6. Jaki jest odstep miedzykwartylowy ceny mieszkan polozonych na pólnocy a jaki tych na poludniu? (Pólnoc i poludnie definiujemy jako polozenie odpowiednio powyzej i ponizej punktu znajdujacego sie w polowie miedzy najmniejsza i najwieksza szerokoscia geograficzna w zbiorze danych)
df %>% 
  summarise(max(lat),min(lat)) #max(lat) = 47.7776, min(lat) = 47.1559

middle = (47.7776+47.1559)/2


df %>% 
  select(lat,price) %>% 
  filter(lat>middle) %>% 
  summarise(quantile(price))
df %>% 
  select(lat,price) %>% 
  filter(lat<middle) %>% 
  summarise(quantile(price))
  

# Odp: Na polnoc od "stodka" odstep miedzykwartylowy wynosi 321000, a na poludnie 122500

# 7. Jaka liczba lazienek wystepuje najczesciej i najrzadziej w nieruchomosciach niepolozonych nad woda, których powierzchnia wewnetrzna na kondygnacje nie przekracza 1800 sqft?
df %>% 
  select(bathrooms,sqft_living,floors,waterfront) %>% 
  filter(waterfront == 0) %>% 
  mutate(sqft_per_floor = sqft_living/floors) %>% 
  filter(sqft_per_floor<=1800) %>% 
  group_by(bathrooms) %>% 
  summarise(count = n()) %>% 
  top_n(1,count) 

df %>% 
  select(bathrooms,sqft_living,floors,waterfront) %>% 
  filter(waterfront == 0) %>% 
  mutate(sqft_per_floor = sqft_living/floors) %>% 
  filter(sqft_per_floor<=1800) %>% 
  group_by(bathrooms) %>% 
  summarise(count = n()) %>% 
  arrange(count) %>% 
  head(1)

# Odp: Najczesciej jest 2.5 lazienki, a najrzadziej 4.75

# 8. Znajdz kody pocztowe, w których znajduje sie ponad 550 nieruchomosci. Dla kazdego z nich podaj odchylenie standardowe powierzchni dzialki oraz najpopularniejsza liczbe lazienek
df %>% 
  select(zipcode) %>% 
  group_by(zipcode) %>% 
  summarise(count = n()) %>% 
  filter(count>550)

df %>% 
  select(zipcode,bathrooms,sqft_lot) %>% 
  filter(zipcode == 98038 | zipcode == 98052 | zipcode == 98103 | zipcode == 98115 | zipcode == 98117) %>% 
  group_by(zipcode) %>% 
  summarise(sd(sqft_lot))

df %>% 
  select(zipcode,bathrooms,sqft_lot) %>% 
  filter(zipcode == 98038) %>% 
  group_by(bathrooms) %>% 
  summarise(count = n()) %>% 
  top_n(1,count)
df %>% 
  select(zipcode,bathrooms,sqft_lot) %>% 
  filter(zipcode == 98052) %>% 
  group_by(bathrooms) %>% 
  summarise(count = n()) %>% 
  top_n(1,count)
df %>% 
  select(zipcode,bathrooms,sqft_lot) %>% 
  filter(zipcode == 98103) %>% 
  group_by(bathrooms) %>% 
  summarise(count = n()) %>% 
  top_n(1,count)
df %>% 
  select(zipcode,bathrooms,sqft_lot) %>% 
  filter(zipcode == 98115) %>% 
  group_by(bathrooms) %>% 
  summarise(count = n()) %>% 
  top_n(1,count)
df %>% 
  select(zipcode,bathrooms,sqft_lot) %>% 
  filter(zipcode == 98117) %>% 
  group_by(bathrooms) %>% 
  summarise(count = n()) %>% 
  top_n(1,count)

# Odp: Kody pocztowe to: 98038, 98052, 98103, 98115, 98117
# Odchylenia standardowe ich powierzchni to odpowiednio: 63111, 10276, 1832, 2675, 2319
# Najpopularniejsza liczba ich lazienek to odpowiednio: 2.5, 2.5, 1, 1, 1

# 9. Porównaj srednia oraz mediane ceny nieruchomosci, których powierzchnia mieszkalna znajduje sie w przedzialach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujacych sie przy wodzie.
df %>% 
  filter(waterfront == 0) %>% 
  select(price, sqft_living) %>% 
  filter(0<sqft_living,sqft_living<=2000) %>% 
  summarise(mean(price),median(price)) # Średnia to 385084.3, a mediana to 359000
df %>% 
  filter(waterfront == 0) %>% 
  select(price, sqft_living) %>% 
  filter(2000<sqft_living,sqft_living<=4000) %>% 
  summarise(mean(price),median(price)) # Średnia to 645419, a mediana to 595000
df %>% 
  filter(waterfront == 0) %>% 
  select(price, sqft_living) %>% 
  filter(4000<sqft_living) %>% 
  summarise(mean(price),median(price)) # Średnia to 1448119, a mediana to 1262750

# Odp: Przy kazdym przedziale

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomosci? (bierzemy pod uwage tylko powierzchnie wewnatrz mieszkania)
df %>% 
  select(price,sqft_living) %>% 
  mutate(price_per_sqft = price/sqft_living) %>%
  transmute(price_per_area = price_per_sqft/0.09290304) %>% 
  summarise(min(price_per_area))

# Odp: 942.7919
