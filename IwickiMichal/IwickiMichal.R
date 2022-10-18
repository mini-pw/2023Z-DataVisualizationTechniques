library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?
df %>%
  filter(bathrooms > median(bathrooms)) %>%
  filter(long > -122) %>%
  pull(price) %>%
  mean()
  
# Odp: 625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?
df %>%
  group_by(yr_built) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(n = 1)

# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?

median(pull(filter(df, waterfront == 1), price)) -> waterfront
median(pull(filter(df, waterfront == 0), price)) -> notwaterfront
(waterfront/notwaterfront-1)*100

# Odp:211.1111%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
df%>%
  filter(floors == 1)%>%
  group_by(yr_built)%>%
  summarise(mprice = min(price))%>%
  pull(mprice)%>%
  mean()   

# Odp: 159999.8

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?
arrange(filter(df,bathrooms == 1))-> one_bath
arrange(filter(df,bathrooms == 2))-> two_bath
quantile(one_bath$grade)
quantile(two_bath$grade)

# Odp:dla 1 lazienki Q1 = 6 Q3 = 7, dla 2 lazienek Q1 = 7 Q3 = 8

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)
filter(df, lat > median(lat))->north
filter(df, lat < median(lat))->south
quantile(south$price)[4] - quantile(south$price)[2]
quantile(north$price)[4] - quantile(north$price)[2]

# Odp:poludnie 218000 polnoc 315000

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
df%>%
  filter(waterfront == 0)%>%
  filter(sqft_living/floors <=  1800)%>%
  count(bathrooms)%>%
  arrange(n)

# Odp: Najczęściej 2.5 a najrzadziej 4.75

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
df%>%
  group_by(zipcode)%>%
  count()%>%
  filter(n>550)%>%
  inner_join(df)-> kody

kody%>%
  count(bathrooms)%>%
  group_by(zipcode)%>%
  filter(n == max(n))

kody%>%
  group_by(zipcode)%>%
  summarise(sd = sd(sqft_lot))

# Odp: 98038 2.5 bathrooms, sd 63111; 
# 98052 2.5 bathrooms, sd 10276; 
# 98103 1 bathroom, sd 1832; 
# 98115 1 bathroom, sd 2675;
# 98117 1 bathroom, sd 2319;

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.
df%>%
  filter(waterfront == 0)%>%
  mutate(przedzial = case_when(
    sqft_living <= 2000 ~ "do 2000",
    sqft_living <= 4000 ~ "od 2000 do 4000",
    TRUE ~ "od 4000"))%>%
  group_by(przedzial)%>%
  summarise(mean = mean(price), median = median(price))

# Odp:              mean    median
# do 2000          385084.  359000
# od 2000 do 4000  645419.  595000
# od 4000         1448119. 1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
df%>%
  summarise(p = price/(sqft_living/0.09290304))%>% #konwersja qsft na metry^2
  pull(p)%>%
  min()  

# Odp: 8.137213 dolarów