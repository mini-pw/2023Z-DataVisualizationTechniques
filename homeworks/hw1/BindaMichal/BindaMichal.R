library(dplyr)

house_data <- read.csv("/Users/michal.binda/IAD/RStudio/semestr2/labki/BindaMichal/Hw1/house_data.csv")
View(house_data)
colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?
house_data %>% 
  filter(bathrooms > median(bathrooms)) %>% 
  filter(long > -122) %>% 
  summarise(mean_price = mean(price, na.rm=TRUE))
  

# Odp: 625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?
house_data %>% 
  group_by(yr_built) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  top_n(1)

# Odp:W ROKU 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
upper_median <- house_data %>% 
  filter(waterfront == 1) %>% 
  summarise(median = median(price))
lower_median <- house_data %>% 
  filter(waterfront == 0) %>% 
  summarise(median = median(price))
(upper_median$median-lower_median$median)/lower_median
# Odp: o 211%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
pom <- house_data %>% 
  filter(floors == 1) %>% 
  group_by(yr_built) %>% 
  filter(price == min(price)) %>% 
  ungroup() %>% 
  select(yr_built, sqft_living) %>% 
  arrange(yr_built)
pom
mean(pom$sqft_living)
?ungroup
# Odp: 1030.422

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?
house_data1 <- house_data %>% 
  filter(bathrooms == 1)
house_data2 <- house_data %>% 
  filter(bathrooms == 2)
df5.1 <- quantile(house_data1$grade)
df5.2 <- quantile(house_data2$grade)
df5.1
df5.2

# z jedną łazienką:
# 1 kwartyl : 6
# 3 kwartyl : 7

# z dwoma:
# 1 kwartyl : 7
# 3 kwartyl : 8

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)
house_data %>% 
  filter(lat == max(lat)) %>% 
  select(lat)
house_data %>% 
  filter(lat == min(lat)) %>% 
  select(lat)
#max lat = 47.7776
#min lat = 47.1559
?mean
mean(c(47.1559,47.7776))
#mean lat = 47.46675
north_houses <- house_data %>% 
  filter(lat > 47.46675)
south_houses <- house_data %>% 
  filter(lat <= 47.46675)

north_houses <- quantile(north_houses$price)
south_houses <- quantile(south_houses$price)
south_houses
north_houses["75%"] - north_houses["25%"]
south_houses["75%"] - south_houses["25%"]
47.6943 - 47.5503
# Odp: na północy : 321000  
      #na południu: 122500  

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
grouped_bath <- house_data %>% 
  filter(sqft_living/floors <= 1800) %>% 
  filter(waterfront == 0) %>% 
  group_by(bathrooms) %>% 
  summarise(n=n()) 

grouped_bath %>% 
  arrange(n) %>% 
  top_n(1)

grouped_bath %>% 
  arrange(-n) %>% 
  top_n(-1)


# Odp: najwiecej domów z 2.5 łąienkami- 4308
#      najmniej domow z 4.75 łazienkami - 1

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
fun <-  function(vector){
  vector %>% 
    summarise(head = n()) %>% 
    arrange(head)
  
}

x <- house_data %>% 
  group_by(zipcode)  %>% 
  summarise(n = n(), sd = sd(sqft_lot)) %>% 
  filter(n>550) %>% 
  
x

?sd
  # Odp:
#  zipcode     n     sd
#1   98038   590 63111.
#2   98052   574 10276.
#3   98103   602  1832.
#4   98115   583  2675.
#5   98117   553  2319.

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.
 house_data %>% 
  filter(waterfront == 0) %>% 
  mutate("grupy" = ifelse(sqft_living <= 2000, "0_2000",
                    ifelse(sqft_living <= 4000, "2000_4000", "4000_+inf"))) %>% 
           group_by(grupy) %>% 
           summarise(mean = mean(price), median = median(price))

# Odp: grupy       mean    median
#    1 0_2000     385084.  359000
#    2 2000_4000  645419.  595000
#    3 4000_+inf 1448119. 1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
x <- house_data %>% 
  mutate("price_m^2" = price/(sqft_living*0.09290304))
x %>% 
  arrange(`price_m^2`) %>% 
  top_n(1) %>% 
  select(`price_m^2`)
# Odp:8720.262
