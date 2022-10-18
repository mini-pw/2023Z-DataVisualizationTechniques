library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie
df
# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

df1 <- filter(df, bathrooms > mean(bathrooms), long > -122)
mean(df1$price)

# Odp: 608237,6

# 2. W którym roku zbudowano najwięcej nieruchomości?
df %>% 
  group_by(yr_built) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
df %>% 
  group_by(waterfront) %>% 
  summarise(med = median(price))

# Odp: (1400000 - 450000) / 450000 = 211,(1)%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
df %>% 
  filter(floors == 1) %>% 
  group_by(yr_built) %>% 
  filter(price == min(price)) %>% 
  group_by() %>% 
  summarise(avArea = mean(sqft_living))

  # Odp: 1030

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

df %>% 
  filter(bathrooms == 1) %>% 
  summarise(q=quantile(grade))

df %>% 
  filter(bathrooms == 2) %>% 
  summarise(q=quantile(grade))

# Odp: 1 kwartyl: 6 i 7, 3 kwartyl 7 i 8, wiec w obydwu przypadkach roznica to 1

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)

df %>% 
  mutate(direction = case_when(lat >= (min(lat) + max(lat))/2 ~ "North",
                               lat < (min(lat) + max(lat))/2 ~ "South")) %>% 
  group_by(direction) %>% 
  summarise(q = quantile(price))

# Odp: polnoc: 711000-390000=321000, poludnie: 367500 - 245000 = 122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
xd <- df %>% 
  filter(waterfront == 0) %>% 
  filter(sqft_living <= 1800 * floors) %>% 
  group_by(bathrooms) %>% 
  summarise(bathroomsCount=n()) %>%
  arrange(bathroomsCount)
xd
arrange(xd, -bathroomsCount)
# Odp: najmniej 4,75 najwiecej 2,5

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
} #https://www.tutorialspoint.com/r/r_mean_median_mode.htm# copypasted

df %>% 
  group_by(zipcode) %>% 
  summarise(n = n(), stddev = sd(sqft_lot), baths = getmode(bathrooms)) %>% 
  filter(n > 550)

#     zipcode stddev baths
# 1   98038   63111.  2.5
# 2   98052   10276.  2.5
# 3   98103   1832.   1  
# 4   98115   2675.   1  
# 5   98117   2319.   1  

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

df %>% 
  filter(waterfront == 0) %>% 
  mutate(areaLevel = case_when(sqft_living <= 2000 ~ "(0, 2000]",
                               sqft_living <= 4000 ~ "(2000,4000]",
                               TRUE ~ "(4000, +Inf)")) %>% 
  group_by(areaLevel) %>% 
  summarise(medianPrice = median(price), avgPrice = mean(price))

# Odp: areaLevel    medianPrice avgPrice
#    1 (0, 2000]    359000      385084.
#    2 (2000,4000]  595000      645419.
#    3 (4000, +Inf) 1262750     1448119.

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df %>% 
  mutate(pricePerSM = price * 10.764/sqft_living) %>% 
  summarise(min = min(pricePerSM))

# Odp: 942.7998

