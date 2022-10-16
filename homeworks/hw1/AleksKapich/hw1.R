library(dplyr)

df <- read.csv("house_data.csv", sep=",")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

df1 <- df %>%
  filter(bathrooms>median(bathrooms)) %>%
  filter(long>-122)
res1 <- mean(df1$price)
# Odp: 625499.42

# 2. W którym roku zbudowano najwięcej nieruchomości?

df2 <- df %>% 
  group_by(yr_built) %>% 
  summarise(TotalNumber=n()) %>% 
  arrange(desc(TotalNumber)) %>% 
  head(1)
res2 <- as.numeric(df2$yr_built)
# Odp: 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
water1 <- df %>% filter(waterfront==TRUE)
median_1 <- median(water1$price)
water0 <- df %>% filter(waterfront==FALSE)
median_0 <- median(water0$price)
res3 <- round(100*median_1/median_0, 2)-100
# Odp: 211.11%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
df4 <- df %>% 
  filter(floors==1) %>%
  group_by(yr_built) %>%
  filter(price==min(price))
res4 <- mean(df4$sqft_living)
# Odp: 1030.422

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?
df5_1b <- quantile(df[df$bathrooms==1,]$grade)
df5_2b <- quantile(df[df$bathrooms==2,]$grade)
q1_diff <- df5_2b[["25%"]]- df5_1b[["25%"]]
q3_diff <- df5_2b[["75%"]]- df5_1b[["75%"]]

# Odp: 
# Nieruchomości z jedną łazienką: 
# 1 kwartyl - 6
# 3 kwartyl - 7
# Nieruchomości z dwoma łazienkami: 
# 1 kwartyl - 7
# 3 kwartyl - 8
# W obu wypadkach różnica wynosi 1


# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)
mid = (max(df$lat)+min(df$lat))/2
north <- df %>% filter(lat>mid)
south <- df %>% filter(lat<mid)
north_ir <- quantile(north$price)[["75%"]]-quantile(north$price)[["25%"]]
south_ir <- quantile(south$price)[["75%"]]-quantile(south$price)[["25%"]]
# Odp: 
# Północ: 321000
# Południe: 122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
df7 <- df %>% 
  filter(waterfront==FALSE) %>% 
  filter(sqft_living/floors<=1800) %>% 
  group_by(bathrooms) %>% 
  summarise(TotalNumber=n()) %>% 
  arrange(desc(TotalNumber))
res7 <- c(df7$bathrooms[1], df7$bathrooms[nrow(df7)])
# Odp: Najczęściej 2.5 łazienki, najrzadziej 4.75 łazienki

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
math_mode <- function(vector){
  tb <- as.data.frame(table(vector)) %>% 
    arrange(desc(Freq)) %>% 
    head(1)
  return(tb$vector)
}

df8 <- df %>% 
  group_by(zipcode) %>%
  summarise(TotalNumber=n(), sd = sd(sqft_lot), mode=math_mode(bathrooms)) %>% 
  filter(TotalNumber>550) %>% 
  select(-TotalNumber)
# Odp:
#   zipcode  sd   mode
#    98038 63111. 2.5  
#    98052 10276. 2.5  
#    98103  1832. 1    
#    98115  2675. 1    
#    98117  2319. 1

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

df9 <- df %>% 
  filter(waterfront==FALSE) %>%
  mutate("Range"=ifelse(sqft_living<=2000, "(0,2000]",
                        ifelse(sqft_living<=4000, "(2000,4000]", "(4000, +Inf)"))) %>% 
  group_by(Range) %>% 
  summarise(mean=mean(price), median=median(price))
# Odp:
#  Range        Mean     Median
# (0,2000]      385084.  359000
# (2000,4000]   645419.  595000
# (4000, +Inf) 1448119. 1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
df10 <- df %>% mutate("price_per_sqm_living"=price/(sqft_living*0.09290304))
res10 <- min(df10$price_per_sqm_living)
# Odp: 942.7919