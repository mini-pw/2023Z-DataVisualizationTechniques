library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?
df_temp <- select(df, price, bathrooms, long) %>% 
  filter(long > -122) %>% 
  filter(bathrooms > median(bathrooms))
  
# Odp:
mean(df_temp$price)
# [1] 782699.5

# 2. W którym roku zbudowano najwięcej nieruchomości?
df_temp <- select(df, id, yr_built) %>% 
  group_by(yr_built) %>% 
  count() %>% 
  arrange(desc(n))
# Odp:
df_temp[1, "yr_built"]
#2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
df_temp <-select(df, price, waterfront) %>% 
  group_by(waterfront) %>% 
  summarise(median = median(price))

# Odp:
(100 * df_temp[2, 2]/ df_temp[1, 2])
# 311.1111

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?

# Odp:
select(df, price, floors, yr_built, sqft_living) %>% 
  filter(floors == 1) %>%
  group_by(yr_built) %>% 
  filter(price == min(price)) %>% 
  summarise(mean_area = mean(sqft_living)) %>% 
  print(n = 115)

# yr_built mean_area
# <int>     <dbl>
#   1     1900       600
# 2     1901       680
# 3     1902      1170
# 4     1903       760
# 5     1904       970
# 6     1905       780
# 7     1906       850
# 8     1907       840
# 9     1908      1060
# 10     1909       940
# 11     1910       890
# 12     1911      1170
# 13     1912       430
# 14     1913      1000
# 15     1914      1220
# 16     1915       760
# 17     1916       880
# 18     1917       690
# 19     1918       900
# 20     1919       670
# 21     1920       640
# 22     1921       840
# 23     1922       610
# 24     1923       820
# 25     1924       860
# 26     1925       840
# 27     1926       730
# 28     1927       800
# 29     1928       730
# 30     1929      1000
# 31     1930       760
# 32     1931       860
# 33     1932       930
# 34     1933       600
# 35     1934       730
# 36     1935      1100
# 37     1936       960
# 38     1937       820
# 39     1938      1510
# 40     1939       830
# 41     1940       900
# 42     1941       580
# 43     1942       780
# 44     1943       730
# 45     1944       790
# 46     1945      1250
# 47     1946       760
# 48     1947       910
# 49     1948      1070
# 50     1949       700
# 51     1950      1080
# 52     1951       520
# 53     1952       550
# 54     1953       730
# 55     1954       860
# 56     1955       910
# 57     1956       910
# 58     1957       870
# 59     1958      1250
# 60     1959       840
# 61     1960       840
# 62     1961       920
# 63     1962      1150
# 64     1963      1080
# 65     1964       800
# 66     1965       902
# 67     1966       670
# 68     1967       650
# 69     1968       828
# 70     1969       900
# 71     1970       880
# 72     1971      1590
# 73     1972       950
# 74     1973       790
# 75     1974      1720
# 76     1975      1200
# 77     1976       620
# 78     1977      1150
# 79     1978       770
# 80     1979      1320
# 81     1980      1170
# 82     1981      1080
# 83     1982      1090
# 84     1983      1070
# 85     1984      1210
# 86     1985      1470
# 87     1986      1420
# 88     1987      1320
# 89     1988      1120
# 90     1989      1540
# 91     1990      1010
# 92     1991      1280
# 93     1992      1530
# 94     1993      1750
# 95     1994       840
# 96     1995      1460
# 97     1996      1290
# 98     1997      1680
# 99     1998      1260
# 100     1999      1600
# 101     2000       580
# 102     2001      1960
# 103     2002      1560
# 104     2003       910
# 105     2004       910
# 106     2005      1350
# 107     2006      1930
# 108     2007       820
# 109     2008      1380
# 110     2009      1239
# 111     2010      1740
# 112     2011      2360
# 113     2012      1260
# 114     2013      1300
# 115     2014      1500

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?
select(df, bathrooms, grade) %>% 
  filter(bathrooms == 1 | bathrooms == 2) %>% 
  group_by(bathrooms) %>% 
  summarize(quantile = c(0.25, 0.5, 0.75, 1),
            q_grades = quantile(grade, c(0.25, 0.5, 0.75, 1)))


# Odp:
# bathrooms quantile q_grades
#   1       Q1          6
#   1       Q3          7
#   2       Q1          7
#   2       Q3          8
"Jest różnica. Więcej pokoi w trzecim kwartylu jest z mieszkań z dwoma łazienkami(różnica jednego punktu). W pierwszym jest więcej z jedną łazienką(różnica jednego punktu)"
# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)

select(df, lat, price)%>%
  mutate(isNorth = lat > mean(c(max(lat), min(lat)))) %>%
  group_by(isNorth) %>%
  summarise(iqr = IQR(price))

# Odp: 
# Południe: 122500,
# Połnoc: 321000 

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
df_temp <- select(df, waterfront, bathrooms, sqft_living, floors) %>% 
  filter(waterfront == 0 & sqft_living/floors <= 1800) %>% 
  count(bathrooms)

# Odp:
df_temp %>% slice_max(n) %>% select(bathrooms)
df_temp %>% slice_min(n) %>% select(bathrooms)
# najrzadziej: 4.75
# najczęściej 2.5

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
select(df, zipcode, sqft_lot, bathrooms) %>%
  group_by(zipcode) %>%
  summarise(n = n(), stand_div_area = sd(sqft_lot), 
            most_freq_bath = which.max(tabulate(bathrooms))) %>%
  filter(n > 550) %>%
  select(-n)

# Odp:
# kod pocztowy| st_div powierzchni |  najpopularniejsza ilość łazienek
#   98038            63111.                   2
#   98052            10276.                   2
#   98103            1832.                    1
#   98115            2675.                    1
#   98117            2319.                    1
  
# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.
select(df, waterfront, price, sqft_living) %>% 
  filter(waterfront == 0) %>% 
  mutate(qliving = ifelse(sqft_living <= 2000, 0, 
                          ifelse(sqft_living > 4000, 2, 1))) %>% 
  group_by(qliving) %>% 
  summarise(mean = mean(price),
            median = median(price))
# Odp:

# pow. mieszkaniowa   mean     median
# (2000,4000]         385084.  359000
# (2000,4000]         645419.  595000
# (4000, +Inf)1       448119. 1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
select(df, sqft_living, price) %>% 
  mutate(price_per_sqmeter = price * 10.7639104/sqft_living) %>% # 10.7639104 = m^2/sqft
  slice_min(price_per_sqmeter)

# Odp:
# 942.7919 USD