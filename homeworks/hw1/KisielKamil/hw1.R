library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartosci NA w zadnej kolumnie

# 1. Jaka jest srednia cena nieruchomosci z liczba lazienek powyzej mediany i polozonych na wschód od poludnika 122W??

df %>%
  filter(bathrooms > median(bathrooms), long > -122) %>%
  summarise(median(price))

# Odp: 563500

# 2. W którym roku zbudowano najwiecej nieruchomosci?

df %>%
  select(id, yr_built) %>%
  count(yr_built) %>% 
  arrange(-n) %>% 
  head(1) %>% 
  select(yr_built)

# Odp: 2014

# 3. O ile procent wieksza jest mediana ceny budynków polozonych nad woda w porównaniu z tymi polozonymi nie nad woda?

median_water <- df %>% filter(waterfront == 1) %>% summarise(median(price))
median_land <- df %>% filter(waterfront == 0) %>% summarise(median(price))
(median_water - median_land)*100/median_land

# Odp: 211.1111%

# 4. Jaka jest srednia powierzchnia wnetrza mieszkania dla najtanszych nieruchomosci posiadajacych 1 pietro (tylko parter) wybudowanych w kazdym roku?

df %>% 
  filter(floors == 1) %>% 
  group_by(yr_built) %>% 
  select(sqft_living) %>% 
  summarise(sqft_living = mean(sqft_living))
  

# Odp:  
"
yr_built sqft_living
<int>       <dbl>
1     1900       1394.
2     1901       1349.
3     1902       1150 
4     1903       1162.
5     1904       1206.
6     1905       1058.
7     1906       1185.
8     1907       1115.
9     1908       1323.
10     1909       1179.
# . with 105 more rows
"

# 5. Czy jest róznica w wartosci pierwszego i trzeciego kwartyla jakosci wykonczenia pomieszczen pomiedzy nieruchomosciami z jedna i dwoma lazienkami? Jesli tak, to jak rózni sie Q1, a jak Q3 dla tych typów nieruchomosci?

df1 <- df %>% filter(bathrooms==1)
q11 <- quantile(df1$grade)[2]
q13 <- quantile(df1$grade)[4]

df2 <- df %>% filter(bathrooms==1)
q21 <- quantile(df2$grade)[2]
q23 <- quantile(df2$grade)[4]

q11
q21

q13
q23

# Odp: Nie ma róznic, oba pierwsze kwartyle wynosza 6, a trzecie 7.

# 6. Jaki jest odstep miedzykwartylowy ceny mieszkan polozonych na pólnocy a jaki tych na poludniu? (Pólnoc i poludnie definiujemy jako polozenie odpowiednio powyzej i ponizej punktu znajdujacego sie w polowie miedzy najmniejsza i najwieksza szerokoscia geograficzna w zbiorze danych)

north <- df %>% arrange(-lat) %>% head(1) %>% select(lat)
south <- df %>% arrange(lat) %>% head(1) %>% select(lat)
center <- (north$lat+south$lat)/2

qn <- df %>% filter(lat >= center)
qn <- quantile(qn$price)
qs <- df %>% filter(lat <= center)
qs <- quantile(qs$price)

IQR(qn)
IQR(qs)

# Odp: Polnoc: 321000USD, Poludnie: 122500USD

# 7. Jaka liczba lazienek wystepuje najczesciej i najrzadziej w nieruchomosciach niepolozonych nad woda, których powierzchnia wewnetrzna na kondygnacje nie przekracza 1800 sqft?

df %>% 
  filter(waterfront == 1, sqft_living/floors <= 1800) %>% 
  count(bathrooms) %>% 
  arrange(n)

# Odp: Najmniejsza - 1.25 i 4.5 (1 raz), a najwieksza - 2.5 (15 razy)

# 8. Znajdz kody pocztowe, w których znajduje sie ponad 550 nieruchomosci. Dla kazdego z nich podaj odchylenie standardowe powierzchni dzialki oraz najpopularniejsza liczbe lazienek

table <- df %>% 
  select(zipcode, id) %>% 
  count(zipcode) %>% 
  filter(n >550) %>%
  select(zipcode)

deviations <- c()

for (code in table$zipcode) {
  deviation <- df %>% filter(zipcode == code) %>% summarise(sd(sqft_lot))
  deviations <- c(deviations, deviation)
}

bathrooms <- c()

for (code in table$zipcode) {
  bathroom <- df %>% filter(zipcode == code) %>% count(bathrooms) %>% arrange(-n) %>% head(1) %>% select(bathrooms)
  bathrooms <- c(bathrooms, bathroom)
}

table$deviations <- deviations
table$bathrooms <- bathrooms
table

# Odp:
"
zipcode deviations bathrooms
1   98038   63111.11       2.5
2   98052   10276.19       2.5
3   98103   1832.009         1
4   98115   2675.302         1
5   98117   2318.662         1
"

# 9. Porównaj srednia oraz mediane ceny nieruchomosci, których powierzchnia mieszkalna znajduje sie w przedzialach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujacych sie przy wodzie.

mean1 <- df %>% filter(waterfront==0, sqft_living <= 2000) %>% summarise(mean(price))
mean2 <- df %>% filter(waterfront==0, sqft_living > 2000, sqft_living < 4000) %>% summarise(mean(price))
mean3 <- df %>% filter(waterfront==0, sqft_living > 4000) %>% summarise(mean(price))

median1 <- df %>% filter(waterfront==0, sqft_living <= 2000) %>% summarise(median(price))
median2 <- df %>% filter(waterfront==0, sqft_living > 2000, sqft_living < 4000) %>% summarise(median(price))
median3 <- df %>% filter(waterfront==0, sqft_living > 4000) %>% summarise(median(price))

c(mean1, mean2, mean3)

c(median1, median2, median3)

# Odp: Srednie: 385084.3, 645370, 1448119, mediany: 359000, 595000, 1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomosci? (bierzemy pod uwage tylko powierzchnie wewnatrz mieszkania)

df %>% 
  mutate(m_living = sqft_living*0.09) %>% 
  mutate(usd_m = price/m_living) %>% 
  arrange(usd_m) %>% 
  head(1) %>% 
  select(usd_m)

# Odp: 973.2026$
