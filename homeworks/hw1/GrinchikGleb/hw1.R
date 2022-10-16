library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartosci NA w zadnej kolumnie

# 1. Jaka jest srednia cena nieruchomosci z liczba lazienek powyzej mediany i polozonych na wschod od poludnika 122W?
df %>%
  filter(bathrooms > median(bathrooms), long > -122) %>%
  summarise(m = mean(price))


# Odp: 625499.4

# 2. W ktorym roku zbudowano najwiecej nieruchomosci?
df %>% 
  group_by(yr_built) %>%
  summarise(n = n()) %>% 
  select(yr_built) %>%
  head(1)


# Odp: 1900

# 3. O ile procent wieksza jest mediana ceny budynkow polozonych nad woda w porownaniu z tymi polozonymi nie nad woda?

df %>% 
  filter(waterfront == 1) %>%
  summarise(m = median(price)) -> d1

df %>%
  filter(waterfront == 0) %>%
  summarise(m = median(price)) -> d2

k = d1[1, 1]/d2[1, 1] * 100 - 100


# Odp: 211.11%

# 4. Jaka jest srednia powierzchnia wnetrza mieszkania dla najtanszych nieruchomosci posiadajacych 1 pietro (tylko parter) wybudowanych w kazdym roku?
df %>% 
  filter(floors == 1) %>% 
  group_by(yr_built) %>%
  summarise(mi = min(sqft_living)) %>%
  summarise(m = mean(mi))


# Odp:  755

# 5. Czy jest roznica w wartosci pierwszego i trzeciego kwartyla jakosci wykonczenia pomieszczen pomiedzy nieruchomosciami z jedna i dwoma lazienkami? Jesli tak, to jak rozni sie Q1, a jak Q3 dla tych typow nieruchomosci?

df %>%
  filter(bathrooms == 1) %>%
  summarise(qs = quantile(grade))

df %>%
  filter(bathrooms == 2) %>%
  summarise(qs = quantile(grade))

# Odp:  Q1: dla jednej lazienki - 6, dla dwoch - 7. Q3: dla jednej lazienki - 7, dla dwoch - 8

# 6. Jaki jest odstep miedzykwartylowy ceny mieszkan polozonych na polnocy a jaki tych na poludniu? (Polnoc i poludnie definiujemy jako polozenie odpowiednio powyzej i ponizej punktu znajdujacego sie w polowie miedzy najmniejsza i najwieksza szerokoscia geograficzna w zbiorze danych)

df %>%
  summarise(m = median(lat)) -> l

lvl = l[1, 1]
df %>% 
  filter(lat > lvl) %>%
  summarise(qs = quantile(price))

df %>% 
  filter(lat < lvl) %>%
  summarise(qs = quantile(price))

# Odp: North: Q2 - Q1 = 125000, Q3 - Q2 = 190000. South: Q2 - Q1 = 77000, Q3 - Q2 = 141000

# 7. Jaka liczba lazienek wystepuje najczesciej i najrzadziej w nieruchomosciach niepolozonych nad woda, ktorych powierzchnia wewnetrzna na kondygnacje nie przekracza 1800 sqft?

df %>%
  filter(waterfront == 0, sqft_living/floors <= 1800) %>%
  group_by(bathrooms) %>%
  summarise(n = n()) %>%
  arrange(n) %>%
  head(1)

df %>%
  filter(waterfront == 0, sqft_living/floors <= 1800) %>%
  group_by(bathrooms) %>%
  summarise(n = n()) %>%
  arrange(n) %>%
  tail(1)


# Odp: najczesciej - 2.5, najrzadziej - 4.75

# 8. Znajdz kody pocztowe, w ktorych znajduje sie ponad 550 nieruchomosci. Dla kazdego z nich podaj odchylenie standardowe powierzchni dzialki oraz najpopularniejsza liczbe lazienek
df %>%
  group_by(zipcode) %>%
  summarise(n = n()) %>%
  filter(n > 550) -> u

p <- u$zipcode

for (i in p){
  print(i)
  df %>%
    filter(zipcode == i) %>%
    summarise(s = sd(sqft_lot)) %>%
    print()
  
  df %>%
    filter(zipcode == i) %>%
    group_by(bathrooms) %>%
    summarise(n = n()) %>%
    arrange(-n) -> op
  print(op[1, 1])
}

# Odp:  98038: sd = 63111.11, n = 2.5, 98052: sd = 10276.19, n = 2.5, 98103: sd = 1832.009, n = 1, 98115: sd = 2675.302, n = 1, 98117: sd = 2318.662, n = 1

# 9. Porownaj srednia oraz mediane ceny nieruchomosci, ktorych powierzchnia mieszkalna znajduje sie w przedzialach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujacych sie przy wodzie.

df %>%
  filter(waterfront == 0) %>%
  mutate(gr = case_when(sqft_living <= 2000 ~ 1,
                        sqft_living <= 4000 ~ 2,
                        TRUE ~ 3)) %>%
  group_by(gr) %>%
  summarise(m = mean(price), med = median(price)) %>%
  View


# Odp: (0, 2000]: mean = 385084.3, median = 359000, (2000,4000]: mean = 645419.0, median = 595000, (4000, +Inf): mean = 1448118.8, median = 1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomosci? (bierzemy pod uwage tylko powierzchnie wewnatrz mieszkania)

df %>%
  mutate(sqm_living = sqft_living/10.764) %>%
  transmute(n = price/sqm_living) %>%
  arrange(n) %>%
  head(1)

# Odp: 942.7998
