library(dplyr)

df <- read.csv("../house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

df %>%
    filter(bathrooms > median(bathrooms) & long > -122.0) %>%
    summarise(mean_price = mean(price))

# Odp: $625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?

df %>%
    group_by(yr_built) %>%
    summarise(n = n()) %>%
    top_n(1, n)

# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?

df %>%
    group_by(waterfront) %>%
    summarise(median_price = median(price)) %>%
    mutate(diff = c(0, max(median_price) / min(median_price) * 100 - 100))

# Odp: O 211%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?

df %>%
    filter(floors == 1) %>%
    group_by(yr_built) %>%
    filter(price == min(price)) %>%
    ungroup() %>%
    summarise(mean_sqft = mean(sqft_living))

# Odp: 1030 sqft

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

df %>%
    filter(bathrooms == 1 | bathrooms == 2) %>%
    group_by(bathrooms) %>%
    summarise(q = quantile(grade, probs=c(0.25, 0.75)))

# Odp: 1 łazienka: q1=6, q3=7
#      2 łazienki: q1=7, q3=8

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)
df %>%
    mutate(lat_cat = case_when(lat > (max(lat)+min(lat))/2 ~ "N", TRUE ~ "S")) %>%
    group_by(lat_cat) %>%
    summarise(iq = IQR(price))


# Odp: Północ $321000, Południe $122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?

df %>%
    mutate(sqft_per_floor = sqft_living / floors) %>%
    filter(sqft_per_floor <= 1800, waterfront == 0) %>%
    count(bathrooms) %>%
    slice(which(n == max(n) | n == min(n)))

# Odp: Najczęściej: 2.50, najrzadziej: 4.75

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
df %>%
    group_by(zipcode) %>%
    summarise(n = n(), std = sd(sqft_lot), most_freq_bathroom = names(which.max(table(bathrooms)))) %>%
    filter(n > 550)

# Odp:
#   zipcode     n    std most_freq_bathroom
#     <int> <int>  <dbl> <chr>             
# 1   98038   590 63111. 2.5               
# 2   98052   574 10276. 2.5               
# 3   98103   602  1832. 1                 
# 4   98115   583  2675. 1                 
# 5   98117   553  2319. 1 

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

df %>%
    filter(waterfront == 0) %>%
    mutate(category=cut(sqft_living, breaks=c(-Inf, 2000, 4000, Inf), labels=c("low","middle","high"))) %>%
    group_by(category) %>%
    summarise(mean_price = mean(price), median_price = median(price))

# Odp:
#   category mean_price median_price
#   <fct>         <dbl>        <dbl>
# 1 low         385084.       359000
# 2 middle      645419.       595000
# 3 high       1448119.      1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
df %>%
    mutate(p_per_sqm = price / (sqft_living * 0.092903)) %>%
    select(p_per_sqm) %>%
    slice_min(p_per_sqm, n = 1)

# Odp: $942.7923
