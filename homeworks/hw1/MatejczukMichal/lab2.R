library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma warto?ci NA w ?adnej kolumnie

# 1. Jaka jest ?rednia cena nieruchomo?ci z liczb? ?azienek powy?ej mediany i po?o?onych na wsch?d od po?udnika 122W?

df %>% filter(bathrooms > median(bathrooms), long > -122) %>% summarise(mean(price))

# Odp: 625499.4

# 2. W kt?rym roku zbudowano najwi?cej nieruchomo?ci?

df %>% group_by(yr_built) %>%  summarise(no_homes = n()) %>% filter(no_homes == max(no_homes)) %>% select(yr_built)

# Odp: 2014

# 3. O ile procent wi?ksza jest mediana ceny budynk?w po?o?onych nad wod? w por?wnaniu z tymi po?o?onymi nie nad wod??

df %>% group_by(waterfront) %>% summarise(median_price = median(price)) %>% summarise(percent = median_price[2]*100/median_price[1]-100) %>% as.data.frame()

# Odp: 211.1111

# 4. Jaka jest ?rednia powierzchnia wn?trza mieszkania dla najta?szych nieruchomo?ci posiadaj?cych 1 pi?tro (tylko parter) wybudowanych w ka?dym roku?

df %>% filter(floors == 1) %>%
  group_by(yr_built) %>% filter(price == min(price)) %>% ungroup() %>% summarise(mean(sqft_living))%>% as.data.frame()

# Odp: 1030.422

# 5. Czy jest r??nica w warto?ci pierwszego i trzeciego kwartyla jako?ci wyko?czenia pomieszcze? pomi?dzy nieruchomo?ciami z jedn? i dwoma ?azienkami? Je?li tak, to jak r??ni si? Q1, a jak Q3 dla tych typ?w nieruchomo?ci?
df %>% filter(bathrooms %in% c(1,2)) %>% group_by(bathrooms) %>% summarise(q1 = quantile(grade, 1/4), q3 = quantile(grade, 3/4))

# Odp:tak, różnią się:
#bathrooms    q1    q3
#       1     6     7
#       2     7     8


# 6. Jaki jest odst?p mi?dzykwartylowy ceny mieszka? po?o?onych na p??nocy a jaki tych na po?udniu? (P??noc i po?udnie definiujemy jako po?o?enie odpowiednio powy?ej i poni?ej punktu znajduj?cego si? w po?owie mi?dzy najmniejsz? i najwi?ksz? szeroko?ci? geograficzn? w zbiorze danych)

(max(df$lat) + min(df$lat))/2 -> midpoint
df %>% mutate(NS = ifelse(lat>midpoint, "N", "S")) %>% group_by(NS) %>% summarise(iqr = IQR(price))

# Odp: N     321000
#      S     122500

# 7. Jaka liczba ?azienek wyst?puje najcz??ciej i najrzadziej w nieruchomo?ciach niepo?o?onych nad wod?, kt?rych powierzchnia wewn?trzna na kondygnacj? nie przekracza 1800 sqft?

df %>%
  filter(waterfront == 0, sqft_living/floors <= 1800) %>%
  group_by(bathrooms) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  summarise(most_frq = bathrooms[n == max(n)], least_frq = bathrooms[n == min(n)])


# Odp:most_frq least_frq
#       2.5      4.75

# 8. Znajd? kody pocztowe, w kt?rych znajduje si? ponad 550 nieruchomo?ci. Dla ka?dego z nich podaj odchylenie standardowe powierzchni dzia?ki oraz najpopularniejsz? liczb? ?azienek

df %>% group_by(zipcode) %>% summarise(n=n()) %>% ungroup() %>% filter(n>550) %>%
  select(zipcode) %>% merge(df) -> big_codes_only

big_codes_only %>% group_by(zipcode) %>% summarise(sd = sd(sqft_lot)) -> deviations
big_codes_only %>% group_by(zipcode, bathrooms) %>% summarise(n = n()) %>% ungroup() %>% group_by(zipcode) %>%
  summarise(most_frq_bth = bathrooms[n == max(n)], .groups = "drop") %>% merge(deviations)

# Odp: zipcode/most_frq_no_bathrooms/sd
#1   98038          2.5 63111.112
#2   98052          2.5 10276.188
#3   98103          1.0  1832.009
#4   98115          1.0  2675.302
#5   98117          1.0  2318.662

# 9. Por?wnaj ?redni? oraz median? ceny nieruchomo?ci, kt?rych powierzchnia mieszkalna znajduje si? w przedzia?ach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajduj?cych si? przy wodzie.

df %>% filter(waterfront == 0) -> df_non_waterfront
df_non_waterfront %>% filter(sqft_living > 0 & sqft_living <= 2000) %>% summarise(mean(price), median(price)) -> low_range
df_non_waterfront %>% filter(sqft_living > 2000 & sqft_living <= 4000) %>%  summarise(mean(price), median(price))-> mid_range
df_non_waterfront %>% filter(sqft_living > 4000) %>% summarise(mean(price), median(price)) -> high_range
rbind(low_range, mid_range, high_range) -> final
rownames(final) <- c("low", "medium", "high")
final

# Odp:    mean(price)   median(price)
#low       385084.3        359000
#medium    645419.0        595000
#high     1448118.8       1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomo?ci? (bierzemy pod uwag? tylko powierzchni? wewn?trz mieszkania)

df %>% transmute(price_per_sqft = price/sqft_living) %>% min() -> price_per_sqft
price_per_sqft/0.09290304 #konwersja na $/m^2

# Odp: 942.7919