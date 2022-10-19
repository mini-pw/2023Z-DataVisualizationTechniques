# install.packages("dplyr")

library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x)
  sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?
df %>%
  filter(bathrooms > median(bathrooms)) %>%
  filter(long > -122) %>%
  summarise(mean = mean(price))

# Odp: 625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?
df %>%
  group_by(yr_built) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(1) %>%
  select(yr_built)

# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
df %>%
  group_by(waterfront) %>%
  summarise(med = median(price)) -> intermediateResult

intermediateResult %>%
  filter(waterfront == 1) %>%
  .$med -> byTheWater

intermediateResult %>%
  filter(waterfront != 1) %>%
  summarise(sum = sum(med)) %>%
  .$sum -> notByTheWater

(byTheWater - notByTheWater) * 100 / byTheWater
(byTheWater - notByTheWater) * 100 / notByTheWater

# Odp: Ja nie wiem jak to się poprawnie przyjmuje
# więc jeśli te które nad wodą to 100%, to wtedy różnica to 67.85714%
# a jeśli te które nie nad wodą to 100%, to wtedy różnica to 211.1111%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
df %>%
  filter(floors == 1) %>%
  group_by(yr_built) %>%
  filter(price == min(price)) %>%
  ungroup() %>%
  summarise(mean = mean(sqft_living))

# Odp: 1030.

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?
df %>%
  filter(bathrooms == 1 | bathrooms == 2) %>%
  group_by(bathrooms) %>%
  summarise(Q1 = quantile(grade)[2], Q3 = quantile(grade)[4])

# Odp: Tak, różni się, Q1 i Q3 są o 1 wartość większe dla nieruchomości z dwoma łazienkami od tych z jedną

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)
df %>%
  mutate(NorS = ifelse(lat > (min(lat) + max(lat)) / 2, "N", "S")) %>%
  group_by(NorS) %>%
  summarise(omk = IQR(price))

# Odp: Dla tych co na północy - 321000, dla tych co na południu - 122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
df %>%
  filter(waterfront != 1) %>%
  filter(sqft_living / floors <= 1800) %>%
  group_by(bathrooms) %>%
  summarise(n = n()) %>%
  filter(n == min(n) | n == max(n))

# Odp: najczęściej - z 2.5 łazienkami, najrzadziej - z 4.75 łazienkami

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
df %>%
  group_by(zipcode) %>%
  summarise(count = n(),
            sd = sd(sqft_lot),
            mpbc = names(head(sort(
              table(bathrooms), decreasing = T
            ), 1))) %>%
  filter(count > 550) %>%
  mutate(count = NULL)

# Odp:
# zipcode sd     mpbc
# 98038   63111. 2.5
# 98052   10276. 2.5
# 98103   1832.  1
# 98115   2675.  1
# 98117   2319.  1

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.
df %>%
  filter(waterfront != 1) %>%
  mutate(
    m_group = case_when(
      0 < sqft_living & sqft_living <= 2000 ~ "0_2000",
      2000 < sqft_living & sqft_living <= 4000 ~ "2000_4000",
      4000 < sqft_living ~ "4000_+inf"
    )
  ) %>%
  group_by(m_group) %>%
  summarise(mean = mean(price), med = median(price))

# Odp:
# m_group   mean     med
# 0_2000    385084.  359000
# 2000_4000 645419.  595000
# 4000_+inf 1448119. 1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
df %>%
  mutate(funt_per_sqm = price / (0.092903 * sqft_living)) %>%
  arrange(funt_per_sqm) %>%
  select(funt_per_sqm) %>%
  head(1)

# Odp: 942.7923